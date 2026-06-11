     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0050R91 - EMPLOYER AUDIT INFORMATION
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE EMPLOYER BILL ADDR FILE. *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0050R - WORK WITH EMPLOYERS                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 04/30/02  PROGRAMMER: C:90  PROJECT: 93WCK          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*  92 - GENERAL F KEY PRESSED                                   *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*                                                               *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  SCR02:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  SCR03:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  WDWEMP#M:                                                    *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*****************************************************************
     FW0050F91  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMEMP     IF   E             DISK
     FWMAIP     UF A E             DISK
     F                                     RENAME(PAI01:DBRCDF)
     FWMAIL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWMFNL     IF   E           K DISK
     FSDCMLD    IF   E           K DISK
     FSMARL     IF   E           K DISK
      *
      * Effective and Cancel Date Arrays
      *
      *
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
      *
     D                 DS
     D  ED                     1     35  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D                 DS
     D  CD                     1     35  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
      *
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
     D                 DS
     D  XXINFO                 1    240
     D  SCINF1                 1     60
     D  SCINF2                61    120
     D  SCINF3               121    180
     D  SCINF4               181    240
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D*
     D OPTNM           C                   CONST('Employer Special Aud-
     D                                     it Information')
     D #OFSCR          C                   CONST(1)
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      EMFND         ##FND
     C                   PARM      EMEMP#        ##EMP#            9 0
     C                   PARM      EMDIV         ##DIV             5 0
     C                   PARM                    ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*
     C*  AUDIT INFORMATION FILE
     C*
     C     AIKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C*  GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C*
     C*  DARTS REMARK DETAIL FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    QCKEY
     C                   KFLD                    XXRMCD
     C*
     C     CMKY2         KLIST
     C                   KFLD                    QCKEY
     C                   KFLD                    XXRMCD
     C                   KFLD                    XXCDAT
     C*
     C     ARKY          KLIST
     C                   KFLD                    QCKEY
     C                   KFLD                    CMSEQ#
     C                   KFLD                    XXASQ#
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@QTE             1
     C*
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    EMEFD1        XXEFDT
     C     *LIKE         DEFINE    EMCAD1        XXCXDT
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    ARASQ#        XXASQ#
     C     *LIKE         DEFINE    CMRMCD        XXRMCD
     C     *LIKE         DEFINE    CMCDAT        XXCDAT
     C*
     C/COPY SCOPYMT01C
     C*
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C*
     C     ERRCK3        BEGSR
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK3
     C*
     C*===============================================================
     C* KUDO - SUBROUTINE FOR F20
     C*===============================================================
     C*
     C*  F20=CHANGE STATUS
     C*
     C     KUDO          BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C*
     C     AISTAT        IFEQ      'A'
     C                   MOVEL     'I'           AISTAT
     C                   MOVEL     'I'           SCSTAT
     C                   ELSE
     C                   MOVEL     'A'           AISTAT
     C                   MOVEL     'A'           SCSTAT
     C                   ENDIF                                                  A3STAT=A
     C*
     C                   MOVEL     Q1USER        AIUSER
     C                   MOVEL     Q1PGM         AIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AIDT
     C                   MOVEL     DSTIME        AITIME
     C*
     C  N60              UPDATE    DBRCDF
     C*
     C                   EXSR      GETDTM
     C*
     C                   ENDSR                                                  KUDO
     C*
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=SEARCH
     C*
     C     KJDO          BEGSR
     C*
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C   40              MOVEL     'A'           AISTAT
     C*
     C                   MOVEL     AIINF1        SCINF1
     C                   MOVEL     AIINF2        SCINF2
     C                   MOVEL     AIINF3        SCINF3
     C                   MOVEL     AIINF4        SCINF4
     C                   MOVEL     AIINF5        SCINF5
     C*
     C                   MOVEL     AISTAT        SCSTAT
     C*
     C*
     C     @@MODE        IFEQ      'A'
     C*
     C* PRE-LOAD 'ASAIN' DARTS INFO (IF ANY)
     C*
     C                   CLEAR                   QCKEY
     C*
     C                   MOVE      *BLANKS       SCINF1
     C                   MOVE      *BLANKS       SCINF2
     C                   MOVE      *BLANKS       SCINF3
     C                   MOVE      *BLANKS       SCINF4
     C                   MOVE      *BLANKS       SCINF5
     C*
     C                   MOVE      EMCO#         QCCO#
     C                   MOVE      EMFND         QCFND
     C                   MOVE      EMEMP#        QCEMP#
     C                   MOVE      EMDIV         QCDIV
     C                   MOVE      *BLANKS       QCQTE
     C                   MOVE      'ASAIN'       XXRMCD
     C*
     C*  CHAIN TO DARTS REMARKS DETAIL FILE
     C*
     C                   Z-ADD     *HIVAL        XXCDAT
     C     CMKY2         SETGT     PCM01
     C     CMKY          READPE    PCM01                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CMCOMM        XXINFO
     C                   Z-ADD     1             XXASQ#
     C     ARKY          CHAIN     PAR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     ARCM75        SCINF5
     C                   ENDIF                                                  *IN60
     C                   ELSE
     C                   MOVE      *BLANKS       XXINFO
     C                   MOVE      *BLANKS       SCINF5
     C                   ENDIF                                                  *IN60
     C*
     C                   ENDIF                                                  @@MODE='A'
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCINF1        AIINF1
     C                   MOVEL     SCINF2        AIINF2
     C                   MOVEL     SCINF3        AIINF3
     C                   MOVEL     SCINF4        AIINF4
     C                   MOVEL     SCINF5        AIINF5
     C*
     C                   MOVEL     SCSTAT        AISTAT
     C*
     C                   ENDSR                                                  MVSCDB
     C*
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C*
     C     OTHERR        BEGSR
     C*
     C     *INKC         CASEQ     *ON           FINISH
     C                   ENDCS
     C*
     C                   ENDSR                                                   OTHERR
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKU         CASEQ     *ON           KUDO                     90
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR                                                  PGMEND
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C     @@RRN         CHAIN     WMEMP                              99
      *
      * Get CX date
      *
     C                   Z-ADD     5             X                 3 0
      *
     C     ED(X)         DOWEQ     *ZERO
     C     X             ANDGT     1
     C                   SUB       1             X
     C                   ENDDO
      *
     C                   Z-ADD     ED(X)         XXEFDT
     C                   Z-ADD     CD(X)         XXCXDT
      *
     C                   Z-ADD     XXCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
      *
     C     SRERR         COMP      *ON                                    50
      *
     C*
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
     C*
     C*  GET AUDIT INFORMATION
     C*
     C                   Z-ADD     *ZEROS        @@RRN
     C*
     C                   CLEAR                   DBRCDF
     C*
     C     AIKY          CHAIN(N)  PAI01                              60
     C*
     C     @@MODE        IFEQ      'C'
     C     *IN60         ANDEQ     *ON
     C                   MOVE      *ON           *IN40
     C                   MOVE      'A'           @@MODE
     C                   ENDIF                                                  @@MODE=C
     C*
     C  N60              Z-ADD     DBRRN         @@RRN
     C*
     C*  GET GROUP INFORMATION
     C*
     C     FNKY          CHAIN(N)  PFN01                              60
     C*
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*  GET AGENT/PRODUCER INFORMATION
     C*
     C                   Z-ADD     *ZEROS        ##FYR
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C*
     C     PREOTH        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B02  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        AIUSER
     C                   MOVEL     Q1PGM         AIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AIDT
     C                   MOVEL     DSTIME        AITIME
     C*
     C                   UPDATE    DBRCDF
     C*
E02  C                   ENDIF                                                  *IN99=*OFF
      *
      *
     C     @@MODE        IFEQ      'C'
     C     SCINF1        ANDEQ     *BLANK
     C     SCINF2        ANDEQ     *BLANK
     C     SCINF3        ANDEQ     *BLANK
     C     SCINF4        ANDEQ     *BLANK
     C     SCINF5        ANDEQ     *BLANK
     C     @@RRN         CHAIN     DBRCDF                             99
     C                   DELETE    DBRCDF
     C                   ENDIF
      *
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITEING DATA BASE RECORDS
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   MOVE      EMCO#         AICO#
     C                   MOVE      EMFND         AIFND
     C                   MOVE      EMEMP#        AIEMP#
     C                   MOVE      EMDIV         AIDIV
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        AIIUSR
     C                   MOVEL     Q1USER        AIUSER
     C                   MOVEL     Q1PGM         AIIPGM
     C                   MOVEL     Q1PGM         AIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AIIDT
     C                   MOVEL     SRDTO         AIDT
     C                   MOVEL     DSTIME        AIITM
     C                   MOVEL     DSTIME        AITIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMAIP
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  WRITER
