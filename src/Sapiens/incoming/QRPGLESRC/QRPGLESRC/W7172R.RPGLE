     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7172R   - RELEASE CX/REINSTATE SPOOL FILE
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE THE   *
     H*                SPOOL FILES FOR A USER ID/AGENT                *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 06/26/06  PROGRAMMER: C:168   PROJECT: 129WNN       *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 03/23/07  C:33  Seq#: WA129/57730                             *
     H*                 Check for valid agent without the group.      *
     H*****************************************************************
     FW7172F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FSMCOL     IF   E           K DISK
     FSMUPP     IF   E             DISK
      *  AGENT NAME
     FWMAAP     IF   E             DISK
      *  AGENT NAME
     F***  Begin Chg  ***  03/23/07  **************************************
     F**********WMAAL1  IF  E           K        DISK
     F**********  PAA01                             KRENAMEPAGL
     FWMAAL5    IF   E           K DISK
     F                                     RENAME(PAA01:PAA05)
     F***  End   Chg  ***  03/23/07  **************************************
      *  User Parameters
     FSMUPL2    IF   E           K DISK
     F                                     RENAME(PUP01:PUP03)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWXAGL1    IF   E           K DISK    USROPN
     F                                     INFDS(DBFDBA)
      *

     D*
     D OPTNM           C                   CONST('Release Cx/Reinstat-
     D                                     e Spool Files')
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDBA          DS
     D  DBRRNA               397    400B 0
      *

     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C***  Begin Del  ***  03/23/07  **************************************
     C**********           KFLD           XXFND
     C***  End   Del  ***  03/23/07  **************************************
     C                   KFLD                    XXAGT#
     C*
     C*   USER PARAMETER FILE KEY LIST
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *===============================================================
      *  *LIKE
      *===============================================================
     C     *LIKE         DEFINE    AACO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    AAAGT#        XXAGT#
     C*
     C                   EXSR      PGMSTR
      *
     C     *IN03         DOWEQ     *OFF
     C                   EXFMT     SCR01
     C                   SETOFF                                       4546
      *
     C                   SELECT
     C     *IN03         WHENEQ    *ON
     C     *IN12         OREQ      *ON
     C                   LEAVE
      * User pressed F10
     C     *IN10         WHENEQ    *ON
     C                   EXSR      KJDO
      *
      *  Validates the screen
     C                   OTHER
     C                   EXSR      ERRCK1
      *
      * No errors were found proceed to next stept
      *
     C     *IN90         IFEQ      *OFF
     C                   EXSR      CHKFIL
     C                   ENDIF
     C                   ENDSL
      *
     C                   ENDDO
      *
     C                   EXSR      PGMEND
      *
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
      *
     C                   SETOFF                                       90
      *
      *  User id must be entered
      *
     C                   MOVEL     *BLANKS       SCUSNM
     C     SCUSR         IFEQ      *BLANKS
     C                   SETON                                        20  90
     C                   ELSE
      *
      *  User id found, display in screen, otherwise give out error
     C     SCUSR         CHAIN     PUP03                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     UPUSR         SCUSR
     C                   MOVEL     UPUSNM        SCUSNM
     C                   ELSE
     C                   SETON                                        21  90
     C                   ENDIF
     C                   ENDIF
      *
      *  Validates the entered Agent range
      *
      * Invalid agent 1
     C                   SELECT
     C     SCAGT1        WHENNE    *ZEROS
     C                   Z-ADD     SCAGT1        XXAGT#
     C***  Begin Chg  ***  03/23/07  **************************************
     C********** AAKY      CHAINWMAAL1               99
     C     AAKY          CHAIN     WMAAL5                             99
     C***  End   Chg  ***  03/23/07  **************************************
     C   99              SETON                                        22  90
     C  N99              Z-ADD     AAAGT#        SCAGT1
      *
      * Invalid agent 2
     C     SCAGT2        WHENNE    *ZEROS
     C                   Z-ADD     SCAGT2        XXAGT#
     C***  Begin Chg  ***  03/23/07  **************************************
     C********** AAKY      CHAINWMAAL1               99
     C     AAKY          CHAIN     WMAAL5                             99
     C***  End   Chg  ***  03/23/07  **************************************
     C   99              SETON                                        23  90
     C  N99              Z-ADD     AAAGT#        SCAGT2
     C                   ENDSL
      *
      * Agent 1 is zero - Agent 2 not zero
     C     *IN90         IFEQ      *OFF
     C     SCAGT1        ANDEQ     *ZEROS
     C     SCAGT2        ANDNE     *ZEROS
     C                   SETON                                        22  90
     C                   ENDIF
      *
      * Agent 1 not zero- Agent 2 is zero
     C     *IN90         IFEQ      *OFF
     C     SCAGT1        ANDNE     *ZEROS
     C     SCAGT2        ANDEQ     *ZEROS
     C                   SETON                                        23  90
     C                   ENDIF
      *
      * Arrange the agent range if one is greater than the other
     C     SCAGT1        IFNE      *ZEROS
     C     SCAGT1        ANDGT     SCAGT2
     C                   Z-ADD     SCAGT1        XXAGT             5 0
     C                   Z-ADD     SCAGT2        SCAGT1
     C                   Z-ADD     XXAGT         SCAGT2
     C                   ENDIF
      *
      * Outq must be entered
      *
     C     SCOUTQ        IFNE      *BLANK
     C     SCAGT1        ORNE      *ZEROS
     C     SCAGT2        ANDNE     *ZEROS
      *
      *  Validates the Outq entered by the user
      *
     C                   CALL      'S0003C'
     C                   PARM                    SCOUTQ
     C                   PARM      ' '           XXFOND            1
     C     XXFOND        IFEQ      '1'
     C                   SETON                                        24  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR                                                  ERRCK1
     C*===============================================================
     C* CHKFIL - CHECKS THE FILE AND OVERRIDES
     C*===============================================================
     C*
     C     CHKFIL        BEGSR
      *
      *  Check file to see if there is a member for selected user
      *
     C     SCUSR         IFNE      *BLANKS
     C                   CALL      'SRCHKMBR'
     C                   PARM      'WXAGP'       QQOBJN           10
     C                   PARM      '*FILE'       QQTYPE            7
     C                   PARM      SCUSR         QQMBR            10
     C                   PARM      *BLANKS       QQERR             1
      *
     C     QQERR         IFEQ      '1'
     C                   SETON                                        25  90
     C                   ENDIF
     C                   ENDIF
      *
     C     *IN90         IFEQ      *OFF
     C                   CALL      'W7172C'
     C                   PARM      SCUSR         QQUSER           10
      *
     C                   OPEN      WXAGL1
     C                   EXSR      PRCSEL
     C                   CLOSE     WXAGL1
     C                   ENDIF
     C*
     C                   ENDSR                                                  ERRCK1
     C*===============================================================
     C* PRCSEL  - PROCESS THE SELECTION BY THE USER
     C*===============================================================
     C*
     C     PRCSEL        BEGSR
      *
     C     *LIKE         DEFINE    AGHDR         XXHDR
     C     *LIKE         DEFINE    AGAGT#        XAGT#
      *
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXHDR
      *
     C     AGKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXHDR
     C                   KFLD                    XAGT#
      *
      * Agent range entered, do it for all agents within range
     C                   SELECT
     C     SCAGT1        WHENNE    *ZEROS
     C     SCAGT2        ANDNE     *ZEROS
     C                   Z-ADD     *ZEROS        XXTOT             9 0
      *
     C                   MOVE      'Y'           XXHDR
     C                   Z-ADD     SCAGT1        XAGT#
      *
     C     AGKY1         SETLL     WXAGL1
     C     AGKY          READE     WXAGL1                                 60
     C     *IN60         DOWEQ     *OFF
      *
     C     AGAGT#        IFGT      SCAGT2
     C                   LEAVE
     C                   ENDIF
      *
     C     AGSTAT        IFNE      'P'
      *
     C                   CALL      'W7174R'
     C                   PARM      'B'           QQMODE            1
     C                   PARM      DBRRNA        QQRRN             9 0
     C                   PARM      AGCO#         QQCO#             3 0
     C                   PARM      AGAGT#        QQAGT#            5 0
     C                   PARM      SCUSR         QQUSER           10
     C                   PARM      SCOUTQ        QQOUTQ           10
     C                   PARM      *BLANKS       QQERR             1
     C                   PARM      *ZEROS        QQREC             9 0
      *
     C                   ADD       QQREC         XXTOT
     C                   ENDIF
      *
     C     AGKY          READE     WXAGL1                                 60
     C                   ENDDO
      *
      * Displays message if we were able to move spool files
      *
     C     XXTOT         IFGT      0
     C                   SETON                                        45
     C                   ELSE
     C                   SETON                                        46
     C                   ENDIF
      *
      * no agent range entered, do it by user id
     C                   OTHER
     C                   CALL      'W7173R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      SCUSR         QQUSER           10
     C                   PARM      SCOUTQ        QQOUTQ           10
     C                   ENDSL
     C*
     C                   ENDSR                                                  ERRCK1
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=TABLE SEARCH
     C*
     C     KJDO          BEGSR
      *
      *  Finds the RRN for the fund to be passed to W0040R
      *
     C     XXFND         IFNE      *ZEROS
     C     FNKEY         CHAIN     PFN01                              99
     C                   Z-ADD     DBRRN         ##RRN
     C                   ELSE
     C                   Z-ADD     *ZEROS        ##RRN
     C                   ENDIF
     C*
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCUSR '
     C                   CALL      'S0005R'
     C                   PARM      '2'           QQCALL            1
     C                   PARM      *ZEROS        QQRRN             9 0
     C                   PARM      1             QQVIEW            1 0
      *
     C     QQRRN         IFNE      *ZEROS
     C     QQRRN         CHAIN     PUP01                              99
     C*                    MOVELUPSTAF    SCCMNU
     C                   MOVEL     UPUSR         SCUSR
     C                   MOVEL     UPUSNM        SCUSNM
     C                   ENDIF
      *
      * Lookup for Agent 1
      *
     C     HFLD          WHENEQ    'SCAGT1'
     C                   CALL      'W0040R'
     C     *INKC         PARM      *ZEROS        QQKC              1
     C     *INKL         PARM      *ZEROS        QQKL              1
     C                   PARM      '2'           QQCALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      3             QQVIEW            1 0
     C                   PARM                    QQQTE             1
      *
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     WMAAP                              99
     C  N99              Z-ADD     AAAGT#        SCAGT1
     C                   ENDIF
      *
      * Lookup for Agent 2
      *
     C     HFLD          WHENEQ    'SCAGT2'
     C                   CALL      'W0040R'
     C     *INKC         PARM      *ZEROS        QQKC              1
     C     *INKL         PARM      *ZEROS        QQKL              1
     C                   PARM      '2'           QQCALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      3             QQVIEW            1 0
     C                   PARM                    QQQTE             1
      *
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     WMAAP                              99
     C  N99              Z-ADD     AAAGT#        SCAGT2
     C                   ENDIF
      *
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C                   MOVE      *ON           *INLR
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
     C                   MOVE      Q1USER        SCUSER
     C*
     C     UPKY          CHAIN     PUP03                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     UPCO#         XXCO#
     C                   Z-ADD     UPFND         XXFND
     C                   ENDIF
     C*
     C*  GET THE COMPANY RECORD
     C*
     C                   MOVE      *BLANKS       SCCONM
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C*                    MOVE *ON       DSPTYP
     C*
     C                   ENDSR                                                  PGMSTR
     C*
