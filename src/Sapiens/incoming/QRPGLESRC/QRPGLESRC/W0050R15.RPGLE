     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0050R15 - EMPLOYER BILLING ADDRESS
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE EMPLOYER BILL ADDR FILE. *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0050R - WORK WITH EMPLOYERS                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 05/15/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 02/08/02 C:61  PROJECT WC577                                  *
     H*                Display CX Date in Header.                     *
     H*                                                               *
     H* 06/04/01 C:61  SUPPORT WA072 27656                            *
     H*              - Make so first address line only required if    *
     H*                some other data field entered.                 *
     H*              - If change mode and all data fields cleared     *
     H*                then delete record.                            *
     H*              - Do not allow F5=Refresh in ADD mode.           *
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
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
     H*  20 - (WCR0503) Address 1 not entered                         *
     H*  21 - (WCR0590) CSZ - Out of Country mutually exclusive       *
     H*  22 - (WCR0504) State not valid                               *
     H*  23 - (WCR0503) Zip 1 cannot be zero                          *
     H*  24 - (WCR0503) City or Out of Country must be entered        *
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
     FW0050F15  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMEMP     IF   E             DISK
     FWMA3P     UF A E             DISK
     F                                     RENAME(PA301:DBRCDF)
     FWMA3L     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWMFNL     IF   E           K DISK
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
      ***** BEG ADD ******* 02/08/02 **********************************
      *
      * Effective and Cancel Date Arrays
      *
      *
      *
      *
      ***** END ADD ******* 02/08/02 **********************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
      ***** BEG ADD ******* 02/08/02 **********************************
      *
      *
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
      ***** END ADD ******* 02/08/02 **********************************
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D*
     D OPTNM           C                   CONST('Employer Billing Add-
     D                                     ress')
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
     C*  BILLING ADDRESS FILE
     C*
     C     A3KY          KLIST
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
     C*  SYSTEM TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@QTE             1
     C*
      ***** BEG ADD ******* 02/08/02 **********************************
      *
      *
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    EMEFD1        XXEFDT
     C     *LIKE         DEFINE    EMCAD1        XXCXDT
      ***** END ADD ******* 02/08/02 **********************************
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
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
      *
      *  ADR1 is required if in change mode and any other data has
      *       been entered.
      *
      ***** BEG ADD ******* 06/04/01 **********************************
     C  N90@@MODE        IFEQ      'C'
     C  N90SCCONT        IFNE      *BLANK
     C     SCADR2        ORNE      *BLANK
     C     SCCTY         ORNE      *BLANK
     C     SCST          ORNE      *BLANK
     C     SCZIP1        ORNE      *ZERO
     C     SCZIP2        ORNE      *ZERO
     C     SCOOC         ORNE      *BLANK
      ***** END ADD ******* 06/04/01 **********************************
     C  N90SCADR1        IFEQ      *BLANK
     C                   SETON                                        20  90
     C                   ENDIF                                                  SCADR1=*BLANK
      ***** BEG ADD ******* 06/04/01 **********************************
     C                   ENDIF
      *
     C                   ENDIF                                                  MODE=C
      *
      *  ADR1 is required in ADD mode.
      *
     C  N90
     CAN 40SCADR1        IFEQ      *BLANK
     C                   SETON                                        20  90
     C                   ENDIF                                                  SCADR1=*BLANK
      *
      ***** END ADD ******* 06/04/01 **********************************
     C*
     C*  CITY, STATE & ZIP CANNOT BE ENTERED ALONG WITH OUT OF COUNTRY
     C*
     C  N90SCCTY         IFNE      *BLANK
     C     SCST          ORNE      *BLANK
     C     SCZIP1        ORNE      *ZERO
     C*
     C     SCOOC         IFNE      *BLANK
     C                   SETON                                        21  90
     C                   ENDIF                                                  SCOOC<>*BLANK
     C*
     C                   ENDIF                                                  SCCTY<>*BLANK
     C*
     C*  THE STATE MUST BE VALID
     C*
     C  N90SCCTY         IFNE      *BLANK
     C*
     C                   MOVEL(P)  SCST          XXKEY            10
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        22  90
     C*
     C*  THE ZIP CODE CANNOT BE <= ZERO
     C*
     C  N90SCZIP1        IFLE      *ZEROS
     C                   SETON                                        23  90
     C                   ENDIF                                                  SCZP1=*BLANK
     C*
     C                   ENDIF                                                  SCCTY<>*BLANKS
     C*
     C*  EITHER CITY/STATE OR OUT OF COUNTRY ADDRESS MUST BE ENTERED
     C*
      ***** BEG ADD ******* 06/04/01 **********************************
     C  N90SCADR1        IFNE      *BLANK
      ***** END ADD ******* 06/04/01 **********************************
     C  N90SCCTY         IFEQ      *BLANK
     C     SCOOC         ANDEQ     *BLANK
     C                   SETON                                        24  90
     C                   ENDIF                                                  SCCTY=*BLANK=SCOOC
      ***** BEG ADD ******* 06/04/01 **********************************
     C                   ENDIF                                                                  OC
      ***** END ADD ******* 06/04/01 **********************************
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90A3DT          IFGT      SAVEDT
     C     A3DT          OREQ      SAVEDT
     C     A3TIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  A3DT>SAVEDT
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
     C     A3STAT        IFEQ      'A'
     C                   MOVEL     'I'           A3STAT
     C                   MOVEL     'I'           SCSTAT
     C                   ELSE
     C                   MOVEL     'A'           A3STAT
     C                   MOVEL     'A'           SCSTAT
     C                   ENDIF                                                  A3STAT=A
     C*
     C                   MOVEL     Q1USER        A3USER
     C                   MOVEL     Q1PGM         A3PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         A3DT
     C                   MOVEL     DSTIME        A3TIME
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
     C   40              MOVEL     'A'           A3STAT
     C*
     C                   MOVEL     A3CONT        SCCONT
     C                   MOVEL     A3ADR1        SCADR1
     C                   MOVEL     A3ADR2        SCADR2
     C                   MOVEL     A3CTY         SCCTY
     C                   MOVEL     A3ST          SCST
     C                   MOVE      A3ZIP1        SCZIP1
     C                   MOVE      A3ZIP2        SCZIP2
     C                   MOVEL     A3OOC         SCOOC
     C                   MOVEL     A3STAT        SCSTAT
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCCONT        A3CONT
     C                   MOVEL     SCADR1        A3ADR1
     C                   MOVEL     SCADR2        A3ADR2
     C                   MOVEL     SCCTY         A3CTY
     C                   MOVEL     SCST          A3ST
     C                   MOVE      SCZIP1        A3ZIP1
     C                   MOVE      SCZIP2        A3ZIP2
     C                   MOVEL     SCOOC         A3OOC
     C                   MOVEL     SCSTAT        A3STAT
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
      ***** BEG ADD ******* 06/04/01 **********************************
     C                   SETON                                        LR
      ***** END ADD ******* 06/04/01 **********************************
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
      ***** BEG ADD ******* 02/08/02 **********************************
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
      ***** END ADD ******* 02/08/02 **********************************
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
     C*  GET BILLING ADDRESS INFORMATION
     C*
     C                   Z-ADD     *ZEROS        @@RRN
     C*
     C                   CLEAR                   DBRCDF
     C*
     C     A3KY          CHAIN(N)  PA301                              60
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
     C*
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
     C                   MOVEL     Q1USER        A3USER
     C                   MOVEL     Q1PGM         A3PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         A3DT
     C                   MOVEL     DSTIME        A3TIME
     C*
     C                   UPDATE    DBRCDF
     C*
E02  C                   ENDIF                                                  *IN99=*OFF
      *
      ***** BEG ADD ******* 06/04/01 **********************************
      *
     C     @@MODE        IFEQ      'C'
     C     SCCONT        ANDEQ     *BLANK
     C     SCADR1        ANDEQ     *BLANK
     C     SCADR2        ANDEQ     *BLANK
     C     SCCTY         ANDEQ     *BLANK
     C     SCST          ANDEQ     *BLANK
     C     SCZIP1        ANDEQ     *ZERO
     C     SCZIP2        ANDEQ     *ZERO
     C     SCOOC         ANDEQ     *BLANK
     C     @@RRN         CHAIN     DBRCDF                             99
     C                   DELETE    DBRCDF
     C                   ENDIF
      *
      ***** END ADD ******* 06/04/01 **********************************
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
     C                   MOVE      EMCO#         A3CO#
     C                   MOVE      EMFND         A3FND
     C                   MOVE      EMEMP#        A3EMP#
     C                   MOVE      EMDIV         A3DIV
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        A3IUSR
     C                   MOVEL     Q1USER        A3USER
     C                   MOVEL     Q1PGM         A3IPGM
     C                   MOVEL     Q1PGM         A3PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         A3IDT
     C                   MOVEL     SRDTO         A3DT
     C                   MOVEL     DSTIME        A3ITM
     C                   MOVEL     DSTIME        A3TIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMA3P
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  WRITER
