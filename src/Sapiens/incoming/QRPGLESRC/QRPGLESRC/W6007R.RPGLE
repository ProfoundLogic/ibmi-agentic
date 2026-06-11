     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE W6007R - PROCESS AUTO CHECK REQUESTS
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     H/DEFINE   WEB
     H*****************************************************************
     H* PROGRAM DESCRIPTIION:                                         *
     H*   Process Auto Check Requests for Daily Batch and             *
     H*   Group Batch.                                                *
     H* THIS PROGRAM:                                                 *
     H*   Reads all Groups for the Company number.                    *
     H*   Browses the Request for Payment for each Group.             *
     H*   If the Request Date is less than or equal to the            *
     H*   entered Process Date,                                       *
     H*     then Group Aux is accessed to obtain the Bank Acct,       *
     H*     the Check Summary file is accessed for Last Chk#,         *
     H*     the Check Request is accessed.  If the record is          *
     H*     not found, the fields are assigned and the record         *
     H*     is written.  If the record is found, all fields           *
     H*     except Updated, Printed, Wasted, & Restart Chk#           *
     H*     are assigned and the record updated.                      *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   MENU                                                        *
     H* CALLS  TO:                                                    *
     H*   none                                                        *
     H* PROGRAMMER: G. JOHNSON                                        *
     H* CREATED 11/03/94        PROJECT: 60AGR                        *
     H*                                                               *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H* 11/04/14 C:65  Redmine:  4719                                 *
     H*                Add compile options.                           *
     H*                                                               *
     H* 09/26/11 C:189 SUPPORT: 76628                                 *
     H*                Auto request check process should set the      *
     H*                7th position of the Daily batch data area      *
     H*                WDAILYA                                        *
     H*                                                               *
     H* 07/27/09 C:136 PROJECT: 114WUK (CLIENT SPECIFIC)              *
     H*                ALLOW FOR XML TYPE TRANSACTIONS                *
     H*                                                               *
     H* 04/07/08 C:15 Project: 25WOO                                  *
     H*               Added code to create Check Schedule records for *
     H*               EFT accounts or any other bank codes if there   *
     H*               is more than one per group.                     *
     H*                                                               *
     H* 09/01/06 C:15  Support: WC135/54458                           *
     H*                Added code to prevent "test" groups from being *
     H*                processed.  This will prevent checks from      *
     H*                automatically printing on a test group.        *
     H*                                                               *
     H* 08/23/06 C:163 Project: 139MCV                                *
     H*                Medical Bill Interface w/FirstHealth           *
     H*                Apply Project: WC1137 08/19/03                 *
     H*                    Added field to prompt screen for the check *
     H*                    mailing date.                              *
     H*                                                               *
     H* 07/15/03 C:132 Project: 126WBL                                *
     H*                                                               *
     H* 03/22/99 C:15 Project: 25WLW                                  *
     H*               Re-implemented pgm from client 60.              *
     H*                                                               *
     H*  12/28/94  C:30  PROJECT: 60AGR                               *
     H*  07/20/95  C:05  PROJECT: Misc Chg requested by MN            *
     H*  11/27/95 C:30 Correction 60ADJ (WC458) per C:15              *
     H*  06/14/96 C:47 PROJECT:  60AMA                                *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS:                                                  *
     H*                                                               *
     H*    10 -  Help Key pressed                                     *
     H*    11 -  Command Key pressed                                  *
     H*    21 -  Process Date Invalid                                 *
     H*    22 -  "From"  Date Invalid (Group)                         *
     H*    23 -   "To"   Date Invalid (Group)                         *
     H*    24 -  Date Range is not Ascending order                    *
     H*    35 -  *ON for Group Batch, *OFF for Daily Batch            *
     H*    40 -  Check Request file (Group or Daily)                  *
     H*    99 -  General File Access                                  *
     H*                                                               *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW6007F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWACPL     UF A E           K DISK    USROPN
     FWAC3L     UF A E           K DISK    USROPN
     F                                     RENAME(PCP01:PCP02)
     F*** Begin chg *** 04/07/08 **************************************
     F*******WARPL     IF   E           K DISK
     FWARPL12   IF   E           K DISK
     F*** End chg ***** 04/07/08 **************************************
     FWMFNL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWSCKL     IF A E           K DISK
      *
     D MG              S             60    DIM(2) CTDATA PERRCD(1)
      *
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
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE REQUEST PAYMENT FILE
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*** Begin add *** 04/07/08 **************************************
     C     RPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RPCKTP
     C*** End add ***** 04/07/08 **************************************
     C*
     C*  KEY LIST FOR THE CHECK MAINTENANCE FILE
     C*
     C     CPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXPRDT
     C                   KFLD                    XXFND
     C*** Begin add *** 04/07/08 **************************************
     C                   KFLD                    ##ACCT
     C                   KFLD                    ##CKTP
     C*** End add ***** 04/07/08 **************************************
     C*
     C*  KEY LIST FOR THE CHECK SUMMARY FILE
     C*
     C     CKKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ##ACCT
     C*
     C*  Entry parm list
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCKTP            1
     C*
     C*  Date conversion parm lists
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
     C*  Work field definitions
     C*
     C     *LIKE         DEFINE    FNCO#         XXCO#
     C     *LIKE         DEFINE    FNFND         XXFND
     C     *LIKE         DEFINE    FNEXDT        XXDSDT
     C     *LIKE         DEFINE    CPPRDT        XXPRDT
     C     *LIKE         DEFINE    CPFRDT        XXFRDT
     C     *LIKE         DEFINE    CPTODT        XXTODT
     C*** Begin add *** 08/19/03 **************************************
     C     *LIKE         DEFINE    CPCMDT        XXCMDT
     C*** End add ***** 08/19/03 **************************************
     C                   MOVE      *BLANK        XXPMT             1
     C*
     C*  Open the appropriate Check Processing file based on the check
     C*   type flag passed into the pgm.
     C*    - QQCKTP = ' ',  OPEN WACPL  (NORMAL CHECKS)
     C*    - QQCKTP = 'G',  OPEN WAC3L  (GROUP CHECKS)
     C*
     C     QQCKTP        IFEQ      ' '
     C                   OPEN      WACPL
     C                   MOVE      '0'           *IN35
     C                   ELSE
     C                   OPEN      WAC3L
     C                   MOVE      '1'           *IN35
     C                   END
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C*
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
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              99
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDSDT
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
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
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C**************************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*****
     C     *IN11         IFEQ      '1'
     C*****
     C*  F3 = Exit
     C*****
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*****
     C*  F13 = Change user parameters
     C*****
     C     *INKM         IFEQ      '1'
     C                   CALL      'W0006R'
     C                   GOTO      TOP
     C                   END
     C*****
     C*  Help requests
     C*****
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C*
     C                   GOTO      STRT
     C                   END
     C*****
     C                   END
     C**************************************************************************
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C******************************
     C*   PROCESS REQUEST SCREEN   *
     C******************************
     C*
     C     SCR01T        TAG
     C*
     C*  Validate the Batch date
     C*
     C                   Z-ADD     SCPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C                   Z-ADD     SCPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPRDT
     C*
     C*  IF GROUP CHECK OPTION HAS BEEN SELECTED, THEN VALIDATE THE
     C*   DATE RANGE FOR THE CHECKS.
     C*
     C     *IN35         IFEQ      '1'
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C*  DATES MUST BE IN ACENDING ORDER
     C*
     C     XXFRDT        IFGT      XXTODT
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*** Begin add *** 08/19/03 **************************************
     C*  Validate the Check Mailing Date
     C*
     C                   Z-ADD     SCCMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*  Check mailing date must be greater than the process date.
     C*
     C                   Z-ADD     SCCMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCMDT
     C     XXPRDT        IFGT      XXCMDT
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*** End add ***** 08/19/03 **************************************
     C*
     C*  SEND A VERIFICATION MESSAGE IF ENTERED DATE DOES NOT EQUAL
     C*  CURRENT SYSTEM DATE
     C*
     C     SCMSG1        IFEQ      *BLANKS
     C     XXPRDT        ANDNE     XXDSDT
     C                   MOVEL     MG(1)         SCMSG1
     C                   MOVEL     MG(2)         SCMSG2
     C                   GOTO      STRT
     C                   END
     C                   MOVE      *BLANKS       SCMSG1
     C                   MOVE      *BLANKS       SCMSG2
     C*****
     C*  All edits have been passed.
     C****
     C*
     C*  Create an entry in the Check Processing file for all groups
     C*    which have a request pmt eligible for processing.
     C*
     C     FNKY          CHAIN     PFN01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     FNFND         XXFND
     C*
     C*  Don't process a group if the expiration date is greater than
     C*    the system date.
     C*
     C     FNEXDT        IFNE      *ZERO
     C     FNEXDT        ANDLT     XXDSDT
     C*** Begin add *** 09/01/06 **************************************
     C     FNTEST        OREQ      'Y'
     C*** End add ***** 09/01/06 **************************************
     C                   GOTO      NEXT
     C                   ENDIF
     C*
     C*  Only process a group if there is a Group Aux record and the
     C*    'Process Check' flag is 'Y'.
     C*
     C     RPKY          CHAIN     PF201                              99
     C     *IN99         IFEQ      *ON
     C     F2CLCK        ORNE      'Y'
     C                   GOTO      NEXT
     C                   ENDIF
     C*
     C*  Check the request payment file for an eligible request.
     C*
     C                   MOVE      'N'           XXPMT
     C     RPKY          CHAIN     PRP01                              99
     C*** Begin add *** 04/07/08 **************************************
     C     NXTCKTP       TAG
     C                   MOVE      'N'           XXPMT
     C*** End add ***** 04/07/08 **************************************
     C     *IN99         DOWEQ     *OFF
     C*
     C     RPTRDT        IFLE      XXPRDT
     C     RPSTAT        ANDEQ     *BLANK
     C*
     C     *IN35         IFEQ      *OFF
     C     RPPGTP        ANDEQ     *BLANK
     C     *IN35         OREQ      *ON
     C     RPPGTP        ANDEQ     'G'
     C                   MOVE      'Y'           XXPMT
     C                   LEAVE
     C                   END
     C*
     C                   END
     C*
     C*** Begin chg *** 04/07/08 **************************************
     C**** RPKY          READE     PRP01                                  99
     C     RPKY1         READE     PRP01                                  99
     C*** End chg ***** 04/07/08 **************************************
     C                   ENDDO
     C*
     C     XXPMT         IFEQ      'Y'
     C*** Begin add *** 09/26/11 **************************************
    C* Call API WA044R to update the 7th position of data area WDAILYA to 'Y'
    C*
     C                   call      'WA044R'
     C                   parm      *blanks       ##rtcd
     C                   parm      *blanks       ##mckl            1
     C                   parm      'Y'           ##upkl            1
    C*
     C*** end   add *** 09/26/11 **************************************
     C*
     C*  Call API to access "Current" Bank Account based on ##CALL value
     C*
     C                   MOVE      XXCO#         ##CO#
     C                   MOVE      XXFND         ##FND
     C                   MOVE      SCPRDT        ##SRDT
      *
      *** BEG CHG ***** 07/27/09 ************************************
     C*****F2CTYP        IFEQ      'V'
     C***********        MOVE      'V'           ##CALL
     C***********        ELSE
     C***********        MOVE      'C'           ##CALL
     C***********        ENDIF
     C     F2CTYP        IFEQ      *BLANK
     C                   MOVE      'C'           ##CALL
     C                   ELSE
     C                   MOVE      F2CTYP        ##CALL
     C                   ENDIF
      *** END CHG ***** 07/27/09 ************************************
     C*
     C                   CALL      'WA023R'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM      'WC'          ##APPL            2
     C                   PARM                    ##SRDT            8
     C                   PARM                    ##ACCT           20
     C                   PARM                    ##CALL            1
     C                   PARM      *BLANK        ##HDCY            1
     C                   PARM      *BLANK        ##CHKQ           10
     C                   PARM      *BLANK        ##FTYP           10
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM      *ZERO         ##EFDT            7
     C*** Begin add *** 04/07/08 **************************************
     C                   PARM      *BLANK        ##CPYQ           10
     C                   PARM      RPCKTP        ##CKTP           10
     C*** End add ***** 04/07/08 **************************************
     C*
     C*  Don't process this group unless a valid bank acct exists.
     C*
     C     ##RTCD        IFEQ      '1'
     C                   GOTO      NEXT
     C                   ENDIF
     C*
     C*  Retrieve the next available check# from the Check Summary file.
     C*    Create a record if one does not exist for the bank acct#.
     C*
     C     CKKY          CHAIN     PCK01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVE      XXCO#         CKCO#
     C                   Z-ADD     1             CKLCHK
     C                   MOVE      ##ACCT        CKACCT
     C                   WRITE     PCK01
     C                   ENDIF
     C*
     C*  Create the Check Processing record in the appropriate file.
     C*
     C  N35CPKY          CHAIN     PCP01                              40
     C   35CPKY          CHAIN     PCP02                              40
     C*
     C                   MOVE      XXCO#         CPCO#
     C                   MOVE      XXFND         CPFND
     C                   MOVE      XXPRDT        CPPRDT
     C                   Z-ADD     CKLCHK        CPCHK#
     C                   MOVE      ##ACCT        CPACCT
     C                   MOVE      *ZEROS        CPFRDT
     C                   MOVE      *ZEROS        CPTODT
     C                   MOVE      Q1USER        CPUSER
     C                   MOVE      Q1PGM         CPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CPDT
     C                   Z-ADD     DSTIME        CPTIME
     C*** Begin add *** 08/19/03 **************************************
     C                   MOVE      XXCMDT        CPCMDT
     C*** End add ***** 08/19/03 **************************************
     C*** Begin add *** 04/07/08 **************************************
     C                   move      ##CKTP        CPCKTP
     C*** End add ***** 04/07/08 **************************************
      *
     C     *IN35         IFEQ      '1'
     C                   MOVE      XXFRDT        CPFRDT
     C                   MOVE      XXTODT        CPTODT
     C                   END
     C*
     C     *IN40         IFEQ      '0'
     C  N35              UPDATE    PCP01
     C   35              UPDATE    PCP02
     C                   ELSE
     C                   MOVE      *ZERO         CPRSTR
     C                   MOVE      *BLANK        CPWAST
     C                   MOVE      *BLANK        CPSTA0
     C                   MOVE      *BLANK        CPSTA1
     C  N35              WRITE     PCP01
     C   35              WRITE     PCP02
     C                   END
     C*
     C                   END
     C*
     C*** Begin add *** 04/07/08 **************************************
     C     RPKY1         SETGT     PRP01
     C     RPKY          READE     PRP01                                  99
     C  N99              GOTO      NXTCKTP
     C*** End add ***** 04/07/08 **************************************
     C*
     C     NEXT          TAG
     C     FNKY          READE     PFN01                                  99
     C                   ENDDO
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C     *IN35         IFEQ      *OFF
     C                   CLOSE     WACPL
     C                   ELSE
     C                   CLOSE     WAC3L
     C                   ENDIF
     C*
     C                   MOVE      '1'           *INLR
     C**************************************************************************
**  TABLE MG
      Date entered is not equal to current system date.
    Change date if necessary and press ENTER to continue.
