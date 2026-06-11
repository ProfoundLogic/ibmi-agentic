     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H************************************************************************
     H* Program:     SYSPLR1 - Spool File Interface - Prompt Pgm
     H* Project:     SY100A
     H* Programmer:  002
     H* Date:        06/10/07
     H************************************************************************
     H* Revisions:
     H*
     H* 02/04/16 C:300 Support: 39703
     H*                Enable opt 5. Convert spool file to PDF and open
     H*
     H* 11/12/13 C:33  Seq#: 84020
     H*                Remove opt 5.
     H*                Change opt 2 to prompt for Outq.
     H*
     H* 12/10/07 C:136 SUPPORT: WA135 61209
     H*                CHANGE RELEASE SPOOL FILE ERROR TO ACCURATE DESCRIBE THE
     H*                ERROR AS A RELEASE ERROR INSTEAD OF A DISPLAY ERROR
     H*
     H************************************************************************
     H* Variables:
     H* QQCALL   into SYSPLR1:
     H*               blank = called from another program, don't display SCR01
     H*           not blank = display SCR01 first
     H************************************************************************
     H* Indicator Usage:
     H*    40    General Usage/Chain
     H* SCR01
     H*    20    Invalid Output Queue/required
     H*    21    User name required
     H*    22    Form Type required
     H*    23    User Data required
     H* SCR02
     H*    06    If no records to select, don't display column separators
     H*           on flag fields
     H*    20    Invalid selection keyed
     H*    21    Error on FAX
     H*    22    Error on Delete
     H*    23    Error on Display
     H*    32    Roll up indicator
     H*    50    Read on SCR02
     H*    60    Read on SPLFRCD
     H*   N71    SCR02 - View 1 (A)
     H*    71    SCR02 - View 2 (B)
     H*    99    Error indicator on call to other programs
     H************************************************************************
     H* File Usage:
     H* SMUPL   (PUP01):      USER
     H* SPOOLL  (SPLFRCD):    OUTPRI/JOBNUM/DATFOP/TIMFOP
     H* SPOOLL1 (SPLFRCD2):   JOBNAM/USRNAM/JOBNUM/FILNAM/FILNUM
     H************************************************************************
     FSYSPLF1   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR02SA:RRN2)
     F                                     SFILE(SCR02SB:RRN2)
     FSMUPL     IF   E           K DISK
     FSPOOLL    IF   E           K DISK
     FSPOOLL1   UF   E           K DISK
     F                                     RENAME(SPLFRCD:SPLFRCD2)

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D***  Begin Add  ***  11/12/13  ************************************
     DChgSplF          PR                  ExtPgm('CHGSPLF')
     D inJobName                     10    const
     D inUserName                    10    const
     D inJob#                         6    const
     D inFileName                    10    const
     D inFile#                        3    const
     D outReturnCode                  1
     D inOutQ                        10    const options(*nopass)

     D/copy sPrString
     D***  End   Add  ***  11/12/13  ************************************
      ***  Begin Add  ***  02/04/16 *************************************
     D/copy sPrAtrium
     D/copy sPrPrprty
      ***  End Add  ***  02/04/16  **************************************

      ***  Begin Add  ***  02/04/16 *************************************
     d strMessage      s             50
     d strTabName      s             50
     d strIaccessURL   s             50
     d strAddress      s            500
     d outURL          s            500
     d conPrefix       s             50
      ***  End Add  ***  02/04/16  **************************************
     C*
      *------------------------*
      *   KEY LISTS
      *------------------------*
     C*  Spool File/FAX Interface File  (SPLFRCD)
     C     SPLKY1        KLIST
     C                   KFLD                    OUTPRI
     C                   KFLD                    JOBNUM
     C                   KFLD                    DATFOP
     C                   KFLD                    TIMFOP
     C*
     C*  Spool File/FAX Interface File  (SPLFRCD2)
     C     SPLKY2        KLIST
     C                   KFLD                    JOBNAM
     C                   KFLD                    USRNAM
     C                   KFLD                    JOBNUM
     C                   KFLD                    FILNAM
     C                   KFLD                    FILNUM
     C*
     C*  User Parameter File
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
      *------------------------*
      *   PARM LISTS
      *------------------------*
     C*** I0022: FAX Program
     C     PL0020        PLIST
     C                   PARM                    QQJOB#
     C                   PARM                    QQJOBN
     C                   PARM                    QQUSRN
     C                   PARM                    QQFILN
     C                   PARM                    QQFIL#
     C                   PARM                    QQTYPE
     C                   PARM                    QQRTCD
     C*
     C*** DSPSPLF:  Display Document/Spool File Program
     C     PLDSPS        PLIST
     C                   PARM                    QQJOBN
     C                   PARM                    QQUSRN
     C                   PARM                    QQJOB#
     C                   PARM                    QQFILN
     C                   PARM                    QQFIL#
     C                   PARM                    QQRTCD
     C*
     C*** DLTSPLF:  Delete Document/Spool File Program
     C     PLDELS        PLIST
     C                   PARM                    QQJOBN
     C                   PARM                    QQUSRN
     C                   PARM                    QQJOB#
     C                   PARM                    QQFILN
     C                   PARM                    QQFIL#
     C                   PARM                    QQRTCD
     C*
     C*** SPOOLR: Spool File/FAX Interface Setup program
     C     PLSETS        PLIST
     C                   PARM                    SCUSNM
     C                   PARM                    QQOUTQ
     C                   PARM                    SCFRTY
     C                   PARM                    SCUSDT
     C                   PARM                    QQMBRN
     C*
     C*
     C*** SPLEMLC:  SEND EMAIL REQUEST FOR SPOOLED FILE
     C     PLEMLS        PLIST
     C                   PARM                    QQJOBN
     C                   PARM                    QQUSRN
     C                   PARM                    QQJOB#
     C                   PARM                    QQFILN
     C                   PARM                    QQFIL#
     C                   PARM      *BLANK        QQRTCD
     C*
     C*** ATCHDRC:  ATTACH SPOOLED FILE TO DARTS
     C     PLADRS        PLIST
     C                   PARM                    QQJOBN
     C                   PARM                    QQUSRN
     C                   PARM                    QQJOB#
     C                   PARM                    QQFILN
     C                   PARM                    QQFIL#
     C                   PARM      *BLANK        QQRTCD
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCALL            1
     C                   PARM                    QQOUTQ           20
     C                   PARM                    QQUSNM           10
     C                   PARM                    QQFRTY           10
     C                   PARM                    QQUSDT           10
     C                   PARM                    QQMBRN           10
     C*
     C*   Initialization Section
     C     *LIKE         DEFINE    RRN2          SRRN2
     C     *LIKE         DEFINE    OUTQLB        QLOUTQ
     C                   TIME                    DSDTTM
     C                   MOVE      *BLANKS       XXERR             1
     C                   MOVE      *ZEROS        XXDATE            6 0
     C                   MOVE      *BLANKS       QZCALL            1
     C                   MOVE      *BLANKS       QQCO#             3
     C                   MOVE      *BLANKS       QQFND             3
     C                   MOVE      *BLANKS       QQFYR             3
     C                   MOVE      *BLANKS       QQEMP#            5
     C                   MOVE      *BLANKS       QQDIV             3
     C                   MOVE      *BLANKS       QQAGT#            5
     C                   MOVE      *BLANKS       QQJOBN           10
     C                   MOVE      *BLANKS       QQUSRN           10
     C                   MOVE      *BLANKS       QQJOB#            6
     C                   MOVE      *BLANKS       QQFILN           10
     C                   MOVE      *BLANKS       QQFIL#            3
     C                   MOVE      *BLANKS       QQRTCD            1
     C                   MOVE      *BLANKS       QQTYPE            1
     C*
     C*   Beginning of processing
     C     TOP           TAG
     C*
     C*  Load SCR01 values
     C     UPKEY         CHAIN     PUP01                              40
     C     *IN40         IFEQ      '1'
     C                   MOVE      *BLANKS       SCOUTQ
     C                   ELSE                                                   ELSE 1 *IN40
     C                   MOVEL     UPOUTQ        SCOUTQ                             Output Queue
     C                   END                                                    END 1 *IN40
     C                   MOVEL     Q1USER        SCUSNM                             User Name
     C                   MOVEL     '*ALL'        SCFRTY                             Form Type
     C                   MOVEL     '*ALL'        SCUSDT                             User Data
     C***/// 6/4/07 ************
     C     QQOUTQ        IFEQ      *BLANKS
     C                   MOVEL     '*ALL'        QQOUTQ
     C                   ENDIF
     C*
     ***//6/4/07****8
     C                   MOVEL     QQOUTQ        SCOUTQ                             DEFAULT OUTQ?
     C*
     C*   Determine how the program was called (whether to display SCR01)
     C     QQCALL        IFEQ      *BLANKS
     C     QQOUTQ        IFEQ      *BLANKS
     C                   MOVEL     '*ALL    '    QQOUTQ
     C                   ENDIF
     **
     C     QQUSNM        IFEQ      *BLANKS
     C                   MOVEL     Q1USER        QQUSNM
     C                   ENDIF
     **
     C     QQFRTY        IFEQ      *BLANKS
     C                   MOVEL     '*ALL    '    QQFRTY
     C                   ENDIF
     **
     C     QQUSDT        IFEQ      *BLANKS
     C                   MOVEL     '*ALL    '    QQUSDT
     C                   ENDIF
     **
     C                   MOVEL     QQOUTQ        SCOUTQ
     C                   MOVEL     QQUSNM        SCUSNM
     C                   MOVEL     QQFRTY        SCFRTY
     C                   MOVEL     QQUSDT        SCUSDT
     C                   GOTO      SCR01T
     C                   END                                                    END QQCALL eq *BLANK
     C*
     C*   Load the record format info for the first time
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   Display all Formats
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      '     '
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02A'
     C     Q2RECF        OREQ      'SCR02CA'
     C     Q2RECF        OREQ      'SCR02SA'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR02A
     C                   EXFMT     SCR02CA
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02B'
     C     Q2RECF        OREQ      'SCR02CB'
     C     Q2RECF        OREQ      'SCR02SB'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR02B
     C                   EXFMT     SCR02CB
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     c************************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     c********************************************
     C*   F3 = Exit                              *
     c********************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*   F5 = REFRESH                       *
     C****************************************
     C     *INKE         IFEQ      '1'
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR02CA
     C                   WRITE     SCR02CB
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
************************************
     C                   MOVEL     '#'           QQCALL
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
************************************
     C*
     C                   MOVE      *LOVAL        OUTPRI
     C                   MOVE      *LOVAL        JOBNUM
     C                   MOVE      *LOVAL        DATFOP
     C                   MOVE      *LOVAL        TIMFOP
     C     SPLKY1        SETLL     SPLFRCD                            60
     C                   EXSR      LOAD2
     C*
     C                   Z-ADD     1             #@$2
     C*
     C                   MOVEL     *BLANKS       Q2RECF
     C*
     C     *IN71         IFEQ      '0'
     C                   MOVEL     'SCR02A'      Q2RECF
     C                   ELSE                                                   ELSE EQ 'A'
     C                   MOVEL     'SCR02B'      Q2RECF
     C                   END                                                    END EQ 'A'
     C*
     C                   END                                                    END *INKE
     C****************************************
     C*   F11 = CHANGE VIEW                  *
     C****************************************
     C     *INKK         IFEQ      '1'
     C*
     C     *IN71         IFEQ      '0'
     C                   MOVE      '1'           *IN71
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02B'      Q2RECF
     C                   ELSE                                                   ELSE EQ 'A'
     C                   MOVE      '0'           *IN71
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02A'      Q2RECF
     C                   END                                                    END EQ 'A'
     C                   GOTO      STRT
     C*
     C                   END                                                    END *INKK
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C                   EXSR      LOAD2
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLUP
     C*******
     C                   END                                                    END CMDKEYS
     c************************************************************************
     C*  Go to Appropriate Screen
     c************************************************************************
     C     Q2RECF        CABEQ     'SCR01'       SCR01T

      /Free
       //***  Begin Add  ***  11/12/13  ************************************
       Clear scToOutQ;
       //***  End   Add  ***  11/12/13  ************************************
      /End-free
     C*
     C     Q2RECF        CABEQ     'SCR02A'      SCR02T
     C     Q2RECF        CABEQ     'SCR02SA'     SCR02T
     C     Q2RECF        CABEQ     'SCR02CA'     SCR02T
     C*
     C     Q2RECF        CABEQ     'SCR02B'      SCR02T
     C     Q2RECF        CABEQ     'SCR02SB'     SCR02T
     C     Q2RECF        CABEQ     'SCR02CB'     SCR02T
     c*****************************
     C*   PROCESS REQUEST SCREEN  *
     c*****************************
     C     SCR01T        TAG
     C*
     C*   Validate OUTPUT QUEUE
     C     SCOUTQ        IFNE      '*ALL'
     C     SCOUTQ        ANDNE     *BLANKS
     C                   CALL      'S0003C'
     C                   PARM                    SCOUTQ
     C                   PARM      ' '           XXERR
     **
     C     XXERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END                                                    END 1 XXERR
     **
     C                   END                                                    END NE *ALL
     **
     C     SCOUTQ        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*   Validate USER NAME
     C     SCUSNM        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END                                                    END EQ *BLANKS
     C*
     C*   Validate FORM TYPE
     C     SCFRTY        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END                                                    END EQ *BLANKS
     C*
     C*   Validate USER DATA
     C     SCUSDT        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END                                                    END EQ *BLANKS
     C*
     C*   After all editing, call SYSPLR to setup for SCR02
     C                   MOVEL     SCOUTQ        QQOUTQ
     C     SCOUTQ        IFNE      '*ALL'
     C                   MOVEL     '*LIBL'       QLOUTQ
     C                   ELSE                                                   ELSE NE '*ALL'
     C                   MOVE      *BLANKS       QLOUTQ
     C                   END                                                    END NE '*ALL'
     C                   MOVE      QLOUTQ        QQOUTQ
     C                   CALL      'SYSPLR'      PLSETS                   99
     C*
     C                   WRITE     SCR99
     C*
     C*   Upon return from SYSPLR, do SCR02 Load
     C*       Initialize Routine
     C                   EXSR      INIT
     C*
     C                   MOVE      *LOVAL        OUTPRI
     C                   MOVE      *LOVAL        JOBNUM
     C                   MOVE      *LOVAL        DATFOP
     C                   MOVE      *LOVAL        TIMFOP
     C     SPLKY1        SETLL     SPLFRCD
     C*
     C*       Clear Subfile
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR02CA
     C                   WRITE     SCR02CB
     C                   MOVE      '0'           *IN04
     C*
     C                   EXSR      LOAD2                                        LOAD RCDS FROM FILE
     C                   GOTO      STRT
     C*
     c*****************************
     C*   PROCESS REQUEST SCREEN  *
     c*****************************
     C     SCR02T        TAG
     C*
     C* * * Edit fields
     C                   MOVE      '0'           ERR
     C*
     C*** Save RRN so that rolling can properly function if completion is marked
     C                   MOVE      RRN2          SRRN2
     C*
     C  N71              READC     SCR02SA                                50
     C   71              READC     SCR02SB                                50
     C*
     C     *IN50         DOWEQ     '0'
     C                   MOVEA     '0000'        *IN(20)
     C*
     C*** Process for invalid option selected
     C     SCSEL2        IFNE      ' '
     C     SCSEL2        IFNE      '2'
     C     SCSEL2        ANDNE     '3'
     C     SCSEL2        ANDNE     '4'
     C     SCSEL2        ANDNE     '5'
     C     SCSEL2        ANDNE     '6'
     C     SCSEL2        ANDNE     '9'
     C                   MOVE      '1'           *IN20
     C                   MOVE      '1'           ERR               1
     C     *IN71         IFEQ      '0'
     C                   UPDATE    SCR02SA
     C     RRN2          CHAIN     SCR02SB                            40
     C                   UPDATE    SCR02SB
     C                   ELSE                                                   ELSE 0 *IN71
     C                   UPDATE    SCR02SB
     C     RRN2          CHAIN     SCR02SA                            40
     C                   UPDATE    SCR02SA
     C                   END                                                    END 0 *IN71
     C                   GOTO      NXT2
     C                   END
     C                   END
     C*
     C*** Process for 3=HOLD
     C     SCSEL2        IFEQ      '3'
     C                   MOVE      JOBNAM        QQJOBN
     C                   MOVE      USRNAM        QQUSRN
     C                   MOVE      JOBNUM        QQJOB#
     C                   MOVE      FILNAM        QQFILN
     C                   MOVE      FILNUM        QQFIL#
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'HLDSPLF'     PLDSPS                 99      CHANGE  Spool File
     C*** Error Check Here
     C*** "Errors occured on display of requested item(s)." *IN23
     C*
     C     QQRTCD        IFEQ      'E'
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           ERR               1
     C     *IN71         IFEQ      '0'
     C                   UPDATE    SCR02SA
     C     RRN2          CHAIN     SCR02SB                            40
     C                   UPDATE    SCR02SB
     C                   ELSE                                                   ELSE 0 *IN71
     C                   UPDATE    SCR02SB
     C     RRN2          CHAIN     SCR02SA                            40
     C                   UPDATE    SCR02SA
     C                   END                                                    END 0 *IN71
     C                   GOTO      NXT2
     C                   END                                                    END EQ 'E'
     C*
     C                   MOVE      ' '           SCSEL2
     C                   MOVEL     *BLANKS       STATUS
     C                   MOVEL     '*HELD   '    STATUS
     C                   MOVE      *ON           *IN43
     C     *IN71         IFEQ      '0'
     C                   UPDATE    SCR02SA
     C     RRN2          CHAIN     SCR02SB                            40
     C                   UPDATE    SCR02SB
     C                   ELSE                                                   ELSE 0 *IN71
     C                   UPDATE    SCR02SB
     C     RRN2          CHAIN     SCR02SA                            40
     C                   UPDATE    SCR02SA
     C                   END                                                    END 0 *IN71
     C                   END                                                    END SEL2 EQ '1'
     C*
     C*** Process for 2=CHG 2
     C     SCSEL2        IFEQ      '2'
      /Free
       //***  Begin Add  ***  11/12/13  ************************************
       If scToOutQ = *blank;
          Dou scToOutQ <> *blank or isCancel = *on;
             Exfmt RcdOutqWdw;
             isErrOutQ = *off;
             If scToOutQ = *blank and isCancel = *off;
                isErrOutQ = *on;
             Endif;
          Enddo;
          If isCancel = *on;
             scToOutQ = *blanks;
             *in50 = *on;
             Iter;
          Endif;
       Endif;

       ChgSplF(jobNam:usrNam:jobNum:filNam:%char(filNum):qqRtCd:scToOutQ);

       //***  End   Add  ***  11/12/13  ************************************
      /End-free
     C***  Begin Del  ***  11/12/13  ************************************
     C**********         MOVE      JOBNAM        QQJOBN
     C**********         MOVE      USRNAM        QQUSRN
     C**********         MOVE      JOBNUM        QQJOB#
     C**********         MOVE      FILNAM        QQFILN
     C**********         MOVE      FILNUM        QQFIL#
     C**********         MOVE      *BLANKS       QQRTCD
     C**********         CALL      'CHGSPLF'     PLDSPS                 99      CHANGE  Spool File
     C***  End   Del  ***  11/12/13  ************************************
     C* Error Check Here
     C* "Errors occured on display of requested item(s)." *IN23
     C*
       //***  Begin Chg  ***  11/12/13  ************************************
     C*****QQRTCD**      IFEQ      'E'
      /Free
       isSflNxtCh = '0';

       If qqRtcd = 'E'
        or qqRtCd = 'O';

          isSflNxtCh = '1';
       //***  End   Chg  ***  11/12/13  ************************************
      /End-free
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           ERR               1
     C     *IN71         IFEQ      '0'
     C                   UPDATE    SCR02SA
     C     RRN2          CHAIN     SCR02SB                            40
     C                   UPDATE    SCR02SB
     C                   ELSE                                                   ELSE 0 *IN71
     C                   UPDATE    SCR02SB
     C     RRN2          CHAIN     SCR02SA                            40
     C                   UPDATE    SCR02SA
     C                   END                                                    END 0 *IN71
     C                   GOTO      NXT2
     C                   END                                                    END EQ 'E'
     C*
      /Free
       //***  Begin Add  ***  11/12/13  ************************************
       outqNm = Upper(scToOutQ);
       //***  End   Add  ***  11/12/13  ************************************
      /End-free
     C                   MOVE      ' '           SCSEL2
     C                   MOVEL     *BLANKS       STATUS
     C                   MOVEL     '*CHANGED'    STATUS
     C                   MOVE      *ON           *IN43
     C     *IN71         IFEQ      '0'
     C                   UPDATE    SCR02SA
     C     RRN2          CHAIN     SCR02SB                            40
     C                   UPDATE    SCR02SB
     C                   ELSE                                                   ELSE 0 *IN71
     C                   UPDATE    SCR02SB
     C     RRN2          CHAIN     SCR02SA                            40
     C                   UPDATE    SCR02SA
     C                   END                                                    END 0 *IN71
     C                   END                                                    END SEL2 EQ '1'
     C*
     C**** Process for 4=Delete
     C     SCSEL2        IFEQ      '4'
      *****
     C*** Save SPOOLP key (for future SETLL) before using this record to delete
      *
     C*** Delete spool file
     C     SPLKY2        DELETE    SPLFRCD2                           40
      *
     C                   MOVE      JOBNUM        QQJOB#
     C                   MOVE      USRNAM        QQUSRN
     C                   MOVE      JOBNAM        QQJOBN
     C                   MOVE      FILNAM        QQFILN
     C                   MOVE      FILNUM        QQFIL#
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'DLTSPLF'     PLDELS                   99    Delete Spool File
     C*** Error Check Here
     C*** "Errors occured on delete of requested item(s)." *IN22
     C*
     C     QQRTCD        IFEQ      'E'
     C                   MOVE      '1'           *IN22
     C                   MOVE      '1'           ERR               1
     C     *IN71         IFEQ      '0'
     C                   UPDATE    SCR02SA
     C     RRN2          CHAIN     SCR02SB                            40
     C                   UPDATE    SCR02SB
     C                   ELSE                                                   ELSE 0 *IN71
     C                   UPDATE    SCR02SB
     C     RRN2          CHAIN     SCR02SA                            40
     C                   UPDATE    SCR02SA
     C                   END                                                    END 0 *IN71
     C                   GOTO      NXT2
     C                   END                                                    END EQ 'E'
      *
     C*** Update screen with *DELETED
     C                   MOVE      ' '           SCSEL2
     C                   MOVEL     '*DELETED'    FILNAM
     C                   MOVE      *BLANKS       USRNAM
     C                   MOVE      *BLANKS       USRDTA
     C                   MOVE      *BLANKS       STATUS
     C                   MOVE      *BLANKS       TOTPAG
     C                   MOVE      *BLANKS       TOTCPY
     C                   MOVE      *BLANKS       FRMTYP
     C                   MOVE      *BLANKS       OUTPRI
     C                   MOVE      *BLANKS       FILNUM
     C                   MOVE      *BLANKS       JOBNAM
     C                   MOVE      *BLANKS       JOBNUM
     C                   MOVE      *BLANKS       FOPDAT
     C                   MOVE      *BLANKS       FOPTIM
     C     *IN71         IFEQ      '0'
     C                   UPDATE    SCR02SA
     C     RRN2          CHAIN     SCR02SB                            40
     C                   UPDATE    SCR02SB
     C                   ELSE                                                   ELSE 0 *IN71
     C                   UPDATE    SCR02SB
     C     RRN2          CHAIN     SCR02SA                            40
     C                   UPDATE    SCR02SA
     C                   END                                                    END 0 *IN71
     C*
     C                   END                                                    END SEL2 EQ '4'
     C*
     C*** Process for 5=Display documents
     C     SCSEL2        IFEQ      '5'
     C                   MOVE      JOBNAM        QQJOBN
     C                   MOVE      USRNAM        QQUSRN
     C                   MOVE      JOBNUM        QQJOB#
     C                   MOVE      FILNAM        QQFILN
     C                   MOVE      FILNUM        QQFIL#
     C                   MOVE      *BLANKS       QQRTCD
     C***  Begin Chg  ***  02/04/16  ************************************
     C*******            CALL      'DSPSPLF'     PLDSPS                 99      Display Spool File
      /Free

       strTabName   = QQFILN;
       strMessage   = 'Click to open ' + %trim(QQFILN) + ' Spool file';
       strIAccessURL= IAccessURL;
       conPrefix    = '/iWAPDFDownload/';

       If strIAccessURL <> *blanks;

          strAddress = %trim(strIAccessURL) + conPrefix
           + %trim(QQFILN) + '.pdf?destType=brws&transPDF=Run'
           + '&jobInfo=' + %trim(QQJOB#)
           + '/' + %trim(QQUSRN)
           + '/' + %trim(QQJOBN)
           + '&fileName=' + %trim(QQFILN)
           + '&fileNumber=' + %trim(QQFIL#)
           + '&saveStatus=*NO&form-charset=utf-8';

          outURL = strTabName + %trim(strAddress);

          Callp LaunchNewTab(strMessage:strTabName:outURL);

       Else;
          QQRTCD = 'E';
       Endif;

      /End-Free
     C***  End Chg  ***  02/04/16  **************************************
     C*** Error Check Here
     C*** "Errors occured on display of requested item(s)." *IN23
     C*
     C     QQRTCD        IFEQ      'E'
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           ERR               1
     C     *IN71         IFEQ      '0'
     C                   UPDATE    SCR02SA
     C     RRN2          CHAIN     SCR02SB                            40
     C                   UPDATE    SCR02SB
     C                   ELSE                                                   ELSE 0 *IN71
     C                   UPDATE    SCR02SB
     C     RRN2          CHAIN     SCR02SA                            40
     C                   UPDATE    SCR02SA
     C                   END                                                    END 0 *IN71
     C                   GOTO      NXT2
     C                   END                                                    END EQ 'E'
     C*
     C                   MOVE      ' '           SCSEL2
     C     *IN71         IFEQ      '0'
     C                   UPDATE    SCR02SA
     C     RRN2          CHAIN     SCR02SB                            40
     C                   UPDATE    SCR02SB
     C                   ELSE                                                   ELSE 0 *IN71
     C                   UPDATE    SCR02SB
     C     RRN2          CHAIN     SCR02SA                            40
     C                   UPDATE    SCR02SA
     C                   END                                                    END 0 *IN71
     C                   END                                                    END SEL EQ '5'
     C*** Process for 6=RELEASE
     C     SCSEL2        IFEQ      '6'
     C                   MOVE      JOBNAM        QQJOBN
     C                   MOVE      USRNAM        QQUSRN
     C                   MOVE      JOBNUM        QQJOB#
     C                   MOVE      FILNAM        QQFILN
     C                   MOVE      FILNUM        QQFIL#
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'RLSSPLF'     PLDSPS                 99      CHANGE  Spool File
     C*** Error Check Here
     C*** "Errors occured on RELEASE of requested item(s)." *IN23
     C*
     C     QQRTCD        IFEQ      'E'
      *** BEG CHG ***** 12/10/07 ************************************
     C*****************    MOVE '1'       *IN23
     C                   MOVE      '1'           *IN24
      *** END CHG ***** 12/10/07 ************************************
     C                   MOVE      '1'           ERR               1
     C     *IN71         IFEQ      '0'
     C                   UPDATE    SCR02SA
     C     RRN2          CHAIN     SCR02SB                            40
     C                   UPDATE    SCR02SB
     C                   ELSE                                                   ELSE 0 *IN71
     C                   UPDATE    SCR02SB
     C     RRN2          CHAIN     SCR02SA                            40
     C                   UPDATE    SCR02SA
     C                   END                                                    END 0 *IN71
     C                   GOTO      NXT2
     C                   END                                                    END EQ 'E'
     C*
     C                   MOVE      ' '           SCSEL2
     C                   MOVEL     *BLANKS       STATUS
     C                   MOVEL     '*RELEASE'    STATUS
     C                   MOVE      *ON           *IN43
     C     *IN71         IFEQ      '0'
     C                   UPDATE    SCR02SA
     C     RRN2          CHAIN     SCR02SB                            40
     C                   UPDATE    SCR02SB
     C                   ELSE                                                   ELSE 0 *IN71
     C                   UPDATE    SCR02SB
     C     RRN2          CHAIN     SCR02SA                            40
     C                   UPDATE    SCR02SA
     C                   END                                                    END 0 *IN71
     C                   END                                                    END SEL EQ '6'
     C*
     C*
     C*** Read next subfile record
     C     NXT2          TAG
     C  N71              READC     SCR02SA                                50
     C   71              READC     SCR02SB                                50
     C                   END                                                    END DOWEQ 0 *IN50
     C*
     C*** Restore RRN so that rolling can properly function
     C                   MOVE      SRRN2         RRN2
     C*
     C     ERR           CABEQ     '1'           STRT
     C*
     C                   MOVEL     *BLANKS       Q2RECF
     C     *IN71         IFEQ      '0'
     C                   MOVEL     'SCR02A  '    Q2RECF
     C                   ELSE                                                   ELSE 0 *IN71
     C                   MOVEL     'SCR02B  '    Q2RECF
     C                   END                                                    END  0 *IN71
     C*
     C                   GOTO      STRT
     C*
     c*****************************
     C*  END THE PROGRAM          *
     c*****************************
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
      *====================================================================*
      * * *   INIT subroutine                                    I N I T ***
      *====================================================================*
     C     INIT          BEGSR
     C                   Z-ADD     0             RRN2              5 0
     C                   MOVE      '0'           *IN71
     C                   ENDSR
      *====================================================================*
      * * *   LOAD2 Subroutine                                 L O A D 2 ***
      *====================================================================*
     C     LOAD2         BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        13            X                 5 0
     C*
     C                   READ      SPLFRCD                                60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C*
     C                   ADD       1             RRN2
     C                   MOVE      *OFF          *IN43
     C*
     C*  Convert Date and Time File Opened
     C                   MOVE      DATFOP        XXDATE
     C     XXDATE        MULT      100.0001      FOPDAT
     C                   MOVE      TIMFOP        FOPTIM
     C*
     C                   MOVE      ' '           SCSEL2
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR02SA
     C                   WRITE     SCR02SB
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END                                                    *IN60 END
     C*
     C                   END                                                    DO 13 END
     C*
     C* * * Initialize subfile if no records to display
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANKS       FILNAM
     C                   MOVE      *BLANKS       JOBNAM
     C                   MOVE      *BLANKS       JOBNUM
     C                   MOVE      *ZEROS        FILNUM
     C                   MOVE      *BLANKS       USRNAM
     C                   MOVE      *BLANKS       FRMTYP
     C                   MOVE      *BLANKS       USRDTA
     C                   MOVE      *BLANKS       STATUS
     C                   MOVE      *ZEROS        TOTPAG
     C                   MOVE      *ZEROS        TOTCPY
     C                   MOVE      *BLANKS       OUTPRI
     C                   MOVE      *ZEROS        FOPDAT
     C                   MOVE      *ZEROS        FOPTIM
     C*** Hidden Fields
     C                   MOVE      *BLANKS       HLDF
     C                   MOVE      *BLANKS       SAVF
     C                   MOVE      *ZEROS        PAGWRT
     C                   MOVE      *ZEROS        STRPAG
     C                   MOVE      *ZEROS        ENDPAG
     C                   MOVE      *ZEROS        LASPAG
     C                   MOVE      *ZEROS        CPYLFT
     C                   MOVE      *BLANKS       OUTQNM
     C                   MOVE      *BLANKS       OUTQLB
     C                   MOVE      *BLANKS       DATFOP
     C                   MOVE      *BLANKS       TIMFOP
     C                   MOVE      *ZEROS        DATFOP
     C                   MOVE      *BLANKS       PGMOPF
     C                   MOVE      *BLANKS       PGMOPL
     C                   MOVE      *BLANKS       PRTTXT
     C                   MOVE      *ZEROS        RCDLEN
     C                   MOVE      *ZEROS        PAGLEN
     C                   MOVE      *ZEROS        PAGWID
     C                   MOVE      *ZEROS        NUMSEP
     C                   MOVE      *BLANKS       FORNAM
     C                   MOVE      *BLANKS       FORLIB
     C                   MOVE      *ZEROS        TOTRCD
     C*
     C                   MOVE      ' '           SCSEL2
     C                   Z-ADD     1             RRN2
     C                   MOVE      *OFF          *IN43
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR02SA
     C                   WRITE     SCR02SB
     C                   END
     C*
     C*** To position screen and cursor
     C                   Z-ADD     RRN2          #@$2
     C                   MOVE      *BLANK        Q2RECF
     C     *IN71         IFEQ      '0'
     C                   MOVEL     'SCR02A'      Q2RECF
     C                   ELSE
     C                   MOVEL     'SCR02B'      Q2RECF
     C                   END
     C*
     C                   ENDSR
