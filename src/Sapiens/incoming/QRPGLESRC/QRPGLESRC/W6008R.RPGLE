     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W6008R - PROCESS AUTO CHECK REQUESTS
     H*****************************************************************
    H* PROGRAM DESCRIPTIION:                                         *
    H* Process Auto Check Mailing Date.                              *
     H*                                                               *
    H* THIS PROGRAM:                                                 *
    H* Update Check Processing Table with Check mail dates.          *
     H*                                                               *
    H* CALLED FROM:                                                  *
    H* MENU - "Auto Chek Mailing Date"                               *
     H*                                                               *
    H* CALLS  TO:                                                    *
    H* None                                                          *
     H*                                                               *
    H* CREATED: 01/09/07  C:182 PROJECT: MC275                       *
     H*                                                               *
    H*****************************************************************
    H* Revisions:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
    H*  INDICATORS:                                                  *
    H*                                                               *
    H*    10 -  Help Key pressed                                     *
    H*    11 -  Command Key pressed                                  *
    H*    21 -  Process Date Invalid                                 *
    H*    24 -  Date Range is not Ascending order                    *
    H*    35 -  *ON for Group Batch, *OFF for Daily Batch            *
    H*    40 -  Check Request file (Group or Daily)                  *
    H*    99 -  General File Access                                  *
     H*                                                               *
     H*****************************************************************
     FW6008F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWACPL     UF   E           K DISK    USROPN
     FWAC3L     UF   E           K DISK    USROPN
     F                                     RENAME(PCP01:PCP02)
     FWMFNL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     F*WSCKL   IF  E           K        DISK

     D*
     D MG              S             60    DIM(2) CTDATA PERRCD(1)
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

    C*
    C*  KEY LIST FOR THE USER PARAMETER FILE
    C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
    C*
    C*  KEY LIST FOR THE COMPANY FILE
    C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
    C*
    C*  KEY LIST FOR THE GROUP MASTER FILE
    C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
    C*
    C*  KEY LIST FOR THE REQUEST PAYMENT FILE
    C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
    C*
    C*  KEY LIST FOR THE CHECK MAINTENANCE FILE
    C*
     C     CPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXPRDT
     C                   KFLD                    XXFND
    C*
    C*  Entry parm list
    C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCKTP            1
    C*
    C*  Date conversion parm lists
    C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
    C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
    C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
    C*
    C*  Work field definitions
    C*
     C     *LIKE         DEFINE    FNCO#         XXCO#
     C     *LIKE         DEFINE    FNFND         XXFND
     C     *LIKE         DEFINE    FNEXDT        XXDSDT
     C     *LIKE         DEFINE    CPPRDT        XXPRDT
     C     *LIKE         DEFINE    CPFRDT        XXFRDT
     C     *LIKE         DEFINE    CPTODT        XXTODT
    C*** Begin add *** 08/19/03 **************************************
     C     *LIKE         DEFINE    CPCMDT        XXCMDT
    C*** End add ***** 08/19/03 **************************************
     C                   MOVE      *BLANK        XXPMT             1
    C*
    C*  Open the appropriate Check Processing file based on the check
    C*   type flag passed into the pgm.
    C*    - QQCKTP = ' ',  OPEN WACPL  (NORMAL CHECKS)
    C*    - QQCKTP = 'G',  OPEN WAC3L  (GROUP CHECKS)
    C*
     C     QQCKTP        IFEQ      ' '
     C                   OPEN      WACPL
     C                   MOVE      '0'           *IN35
     C                   ELSE
     C                   OPEN      WAC3L
     C                   MOVE      '1'           *IN35
     C                   END
    C*
    C*  GET THE CLIENT NO#
    C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C     XXCLT#        IFEQ      '135'
     C                   SETON                                        22
     C                   ENDIF
    C*
    C*  GET THE USER PARAMETER FILE
    C*
     C     TOP           TAG
    C*
     C                   MOVE      *ZEROS        XXCO#
     C                   MOVE      *ZEROS        SCPRDT
     C     UPKY          CHAIN     PUP01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVE      UPCO#         XXCO#
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPRDT
     C                   END
     C                   MOVE      *BLANKS       SCMSG1
     C                   MOVE      *BLANKS       SCMSG2
    C*
    C*  GET THE COMPANY RECORD
    C*
     C     COKY          CHAIN     PCO01                              99
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
    C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDSDT
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
    C*
    C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
    C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
    C*
    C*   DISPLAY ALL FORMATS
    C*
     C     STRT          TAG
    C*
    C* IF CLIENT IS 135 THAN SHOW GENERATE CONCENTRA PAID DATE FILE FLAG
    C*
     C     XXCLT#        IFEQ      '135'
     C                   SETON                                        22
     C                   ENDIF
    C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
    C*
    C**************************************************************************
    C*   PROCESS ALL COMMAND KEYS
    C*****
     C     *IN11         IFEQ      '1'
    C*****
    C*  F3 = Exit
    C*****
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
    C*****
    C*  F13 = Change user parameters
    C*****
     C     *INKM         IFEQ      '1'
     C                   CALL      'W0006R'
     C                   GOTO      TOP
     C                   END
    C*****
    C*  Help requests
    C*****
     C     *IN10         IFEQ      '1'
    C*
     C                   MOVE      Q2RECF        XXRECF
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
    C*
     C                   GOTO      STRT
     C                   END
    C*****
     C                   END                                                    End of function keys
    C**************************************************************************
    C*   GOTO THE APPROPRIATE SCREEN
    C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
    C*
    C******************************
    C*   PROCESS REQUEST SCREEN   *
    C******************************
    C*
     C     SCR01T        TAG
    C*
    C*  Validate the Batch date
    C*
     C                   Z-ADD     SCPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C                   Z-ADD     SCPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPRDT
    C*
    C*  Validate the Batch date
    C*
     C                   Z-ADD     SCCMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
    C*
    C*  Check mailing date must be greater than the process date.
    C*
     C                   Z-ADD     SCCMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCMDT
     C     XXPRDT        IFGT      XXCMDT
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
    C*
   C* Check Generarte Concentra Paid Date File Flag
   C* valid Values are 'Y' and Blank.
    C*
     C     SCCPDF        IFEQ      'Y'
     C     SCCPDF        OREQ      ' '
    C*
     C                   ELSE
    C*
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
    C*
     C                   ENDIF
    C*
    C*  SEND A VERIFICATION MESSAGE IF ENTERED DATE DOES NOT EQUAL
    C*  CURRENT SYSTEM DATE
    C*
     C     SCMSG1        IFEQ      *BLANKS
     C     XXPRDT        ANDNE     XXDSDT
     C                   MOVEL     MG(1)         SCMSG1
     C                   MOVEL     MG(2)         SCMSG2
     C                   GOTO      STRT
     C                   END
     C                   MOVE      *BLANKS       SCMSG1
     C                   MOVE      *BLANKS       SCMSG2
    C*****
    C*  All edits have been passed.
    C****
    C*
   C*  Update the Check Processing file for all groups with check mailing
    C*  date .
    C*
     C     FNKY          CHAIN     PFN01                              99
    C*
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     FNFND         XXFND
    C*
    C*  Don't process a group if the expiration date is greater than
    C*    the system date.
    C*
     C     FNEXDT        IFNE      *ZERO                                        Group Expiration DT
     C     FNEXDT        ANDLT     XXDSDT
     C                   GOTO      NEXT
     C                   ENDIF
    C*
    C*  Only process a group if there is a Group Aux record and the
    C*    'Process Check' flag is 'Y'.
    C*
     C     RPKY          CHAIN     PF201                              99
     C     *IN99         IFEQ      *ON
     C     F2CLCK        ORNE      'Y'                                          Process Claims
     C                   GOTO      NEXT
     C                   ENDIF
    C*
    C*
    C*
    C*  Update the Check Processing record with check mailing date.
    C*
     C     *IN35         IFEQ      '0'
    C*
     C     CPKY          CHAIN     PCP01                              40
    C*
     C                   ELSE
    C*
     C     CPKY          CHAIN     PCP02                              40
    C*
     C                   END
    C*
     C                   MOVE      Q1USER        CPUSER
     C                   MOVE      Q1PGM         CPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CPDT
     C                   Z-ADD     DSTIME        CPTIME
     C                   MOVE      XXCMDT        CPCMDT
    C*
     C     *IN40         IFEQ      '0'
    C*
     C     *IN35         IFEQ      '0'
    C*
     C                   UPDATE    PCP01
    C*
     C                   ELSE
    C*
     C                   UPDATE    PCP02
    C*
     C                   END
    C*
     C                   END
    C*
    C*
     C     NEXT          TAG
     C     FNKY          READE     PFN01                                  99
     C                   ENDDO
    C*
   C*  Generate Concentra Paid Date File if Flag is 'Y'              ling
    C*
     C     SCCPDF        IFEQ      'Y'
     C                   Z-ADD     XXPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
    C*
     C                   MOVE      XXCO#         CO##              3
     C                   CALL      'F8264C'
     C                   PARM                    CO##
     C                   PARM                    SRDTO
    C*
     C                   ENDIF
    C*
    C*  END THE PROGRAM
    C*
     C     TERM          TAG
     C     *IN35         IFEQ      *OFF
     C                   CLOSE     WACPL
     C                   ELSE
     C                   CLOSE     WAC3L
     C                   ENDIF
    C*
     C                   MOVE      '1'           *INLR
    C**************************************************************************
**  TABLE MG
      Date entered is not equal to current system date.
    Change date if necessary and press ENTER to continue.
