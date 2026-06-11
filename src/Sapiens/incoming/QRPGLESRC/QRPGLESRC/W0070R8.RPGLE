     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W0070R8 - AVERAGE WEEKLY WAGE HISTORY
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*
     H*****************************************************************
     H*  PROGRAM DESCRIPTIONS:                                        *
     H*     THIS PROGRAM SEARCHES THE AVERAGE WEEKLY WAGE FILE.       *
     H*                                                               *
     H*  CALLED BY: W0070R5 - EDI/NOI MAINTENANCE                     *
     H*             W0070R11- CLAIMS, BENEFIT INFO (F18)              *
     H*                                                               *
     H*  CALLS TO:  NONE                                              *
     H*                                                               *
     H*  PROGRAMMER: C:46                                             *
     H*  CREATED:   04/17/96           PROJECT: 68AMN                 *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  06/09/20 C:318 Project: SY420
     H*                 Mask SSN
     H*
     H* 08/26/09 C:232 Support#: 69533                                *
     H*                Compilation only.                              *
     H*                                                               *
     H* 04/22/99 C:118 Project: WC577
     H*                Changed display file so 'Effective Date' is
     H*                not required. Deleted SCPGM and SCUSER.
     H*
     H* 06/16/98 C:15 Project: WC577                                  *
     H*               Changed SCFND to be loaded with XXFND instead   *
     H*               of UPFND.  This was causing a problem with the  *
     H*               delete function.                                *
     H*                                                               *
     H* 02/24/98 C:85 Project: WC577                                  *
     H*               CHANGED F12=CANCEL FUNCTION (QQRTCD TO 'B').    *
     H*                                                               *
     H* 01/15/98 C:15 Project: WC577                                  *
     H*               Chgd pgm to prevent errors when no records are  *
     H*               selected to be displayed on the screen.         *
     H*                                                               *
     H* 08/21/96 C:58 PROJECT: 68AMN                                  *
     H*               Miscellaneous problems: positioning, update with*
     H*               errors, inquiry mode=input-capable, abend if    *
     H*               more than 1 page of records, no RB fields.      *
     H*               Re-wrote RPG and DSPF.                          *
     H*                                                               *
     H* 08/08/96 C:65 PROJECT: 68AMN                                  *
     H*               ADDED FIELD FOR EFFECTIVE DATE                  *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*   KC       Exit                                               *
     H*   KL       Prior screen                                       *
     H*   KX       CONFIRM DELETION                                   *
     H*                                                               *
     H*   21       OPTION INVALID                                     *
     H*   22       DATE INVALID                                       *
     H*   28       NO ERRORS - END NEXT TIME                          *
     H*   29       AT LEAST 1 ERROR IN SUBFILE                        *
     H*   50       SUBFILE CLEAR                                      *
     H*  N50       DISPLAY SUBFILE                                    *
     H*   67       EOF SUBFILE                                        *
     H*   66       No record found WMAWL CHAIN                        *
     H*   68       Excess REIN message                                *
     H*   69       EOF WMAWL                                          *
     H*   81       READ NEXT TIME AS IF CHANGED                       *
     H*   83       Change(s) occurred: rebuild subfile                *
     H*****************************************************************
     FW0070F8   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:RRN)
     F                                     INFDS(FIDS01)
     FWMUPL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWMCDL     IF   E           K DISK
     FWMAWL     IF   E           K DISK
     FWMAWL1    UF   E           K DISK
     F                                     RENAME(PAW01:PAW02)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMEML     IF   E           K DISK
      *

     D PRDS            S             40    DIM(1) CTDATA PERRCD(1)
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D XCEXRE          C                   CONST('EXCS REIN ')
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
      *
      /EJECT
      * ============================================================
      *        M A I N L I N E
      * ============================================================

     C     STRT          TAG
      *
      *   Repeat until 1 display with no errors
     C     *IN29         DOUEQ     *OFF
     C     *IN28         ANDEQ     *OFF
      *
      *   No errors - end next time
     C                   MOVE      *IN29         *IN28
      *
      *   Something changed - reload subfile if all errors cleared
     C     *IN83         IFEQ      *ON
     C     *IN29         ANDEQ     *OFF
     C                   EXSR      LOAD
     C                   ENDIF                                                   *83 = ON
      *
      *   Display subfile screen
     C                   Z-ADD     *ZEROS        SCPOS
     C*** BEGIN ADD *** 01/15/98 ***********************************************
     C                   MOVE      *OFF          *IN59
     C*** END ADD ***** 01/15/98 ***********************************************
     C                   WRITE     SCR01                                        CMD KEYS
     C                   EXFMT     SCR01C
     C*** BEGIN ADD *** 01/15/98 ***********************************************
     C                   MOVE      *ON           *IN59
     C*** END ADD ***** 01/15/98 ***********************************************
      *
      *      Command keys and request functions
      *   Reposition the list
     C     SCPOS         IFNE      0
     C                   EXSR      LOAD
     C                   ITER
     C                   ENDIF                                                   POS ne 0
      *
      *   F3 - End of Function
     C     *INKC         IFEQ      *ON
     C                   MOVE      'A'           QQRTCD
     C                   LEAVE
     C                   ENDIF                                                   *KC = ON
      *
      *   F12 - Previous Screen
     C     *INKL         IFEQ      *ON
      *
     C*** BEGIN DLT *** 01/15/98 ***********************************************
     C********** Q2RECF    ANDEQ'SCR01'
     C*** END DLT ***** 01/15/98 ***********************************************
     C*** BEGIN ADD *** 01/15/98 ***********************************************
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C*** END ADD ***** 01/15/98 ***********************************************
     C*** BEGIN CHG *** 02/24/98 ***********************************************
     C**********           MOVE 'A'       QQRTCD
     C                   MOVE      'B'           QQRTCD
     C*** END   CHG *** 02/24/98 ***********************************************
     C                   LEAVE
     C*** BEGIN ADD *** 01/15/98 ***********************************************
     C                   ENDIF
     C*** END ADD ***** 01/15/98 ***********************************************
      *
     C                   ENDIF                                                   *KL = ON
      *
      *   Edit the subfile
     C                   EXSR      SC001
      *
     C                   ENDDO
      * ============================================================
      *    TERM   End the program
      * ============================================================
      *
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C                   RETURN
      *
      * ============================================================
      *   SUBROUTINE - SC001 Edit subfile data
      * ============================================================
      *
      *   Read all changed subfile records
     C     SC001         BEGSR
     C                   MOVE      *OFF          *IN21                           Reset
     C                   MOVE      *OFF          *IN22
     C                   MOVE      *OFF          *IN29                              Error
     C                   MOVE      *OFF          *IN67
     C                   READC     SCR01S                                 67
      *
     C     *IN67         DOWEQ     *OFF
      *
      *   No selection made.
     C     SCOPT         IFEQ      *BLANK                                        Chg = blank
     C                   MOVE      *OFF          *IN81                           forget it
      *
      *   Process Deletion.
     C                   ELSE                                                    x OPT blank
     C     SCOPT         IFEQ      '4'                                           Delete
     C                   EXSR      DELET
      *
      *   Option not valid
     C                   ELSE                                                    x opt not 4
     C                   MOVE      *ON           *IN21                           Invalid opt
     C                   MOVE      *ON           *IN29                           1 + error
     C                   MOVE      *ON           *IN81                           Next read
     C                   ENDIF                                                   Changed
     C                   ENDIF                                                   NE Blank
      *
      *   Effective date changed
     C     SCEFDT        IFNE      SFEDT                                         Changed
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
      *
      *   Error - effective date invalid
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN29                             1 + Error
     C                   MOVE      *ON           *IN22                             Error
     C                   MOVE      *ON           *IN81                             Rd again
      *
      *   Effective date good - update database
     C                   ELSE                                                    x ERR=ON
     C                   MOVE      *OFF          *IN22                              No error
     C                   MOVE      *OFF          *IN81                              No next
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1TRDT
     C     SFKY          CHAIN     PAW02                              66
     C     *IN66         IFEQ      *OFF
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AWEFDT
     C*
     C                   MOVEL     Q1USER        AWUSER
     C                   MOVEL     Q1PGM         AWPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AWDT
     C                   MOVEL     DSTIME        AWTIME
     C*
     C                   UPDATE    PAW02
     C                   MOVE      *ON           *IN83                           Update SFL
     C                   ENDIF                                                   *66 = OFF
     C                   ENDIF                                                   ERR = ON
      *
     C                   ENDIF                                                   SCEDT/SFEDT
      *
     C                   Z-ADD     SCEFDT        SFEDT                           Hidden
     C                   UPDATE    SCR01S
      *
     C                   READC     SCR01S                                 67
     C                   ENDDO                                                   *67 = OFF
      *
     C                   ENDSR
      * ============================================================
      *   SUBROUTINE - LOAD AWW history records
     C* ============================================================
     C     LOAD          BEGSR
     C                   MOVE      *BLANK        SCOPT
      *
      *   Clear subfile
     C                   MOVE      *ON           *IN50
     C                   WRITE     SCR01C
     C                   MOVE      *OFF          *IN50
      *
     C                   Z-ADD     0             RRN
     C                   Z-ADD     SCPOS         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT
      *
      *   Retrieve the Average Weekly Wage Record
     C     AWKY          SETLL     PAW01                              59
     C     AWKY1         READE     PAW01                                  69
      *
     C     *IN69         DOWEQ     *OFF
     C                   Z-ADD     AWTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTRDT
     C                   Z-ADD     AWAWW         SCAWW
     C                   Z-ADD     AWTIME        SCTIME
     C                   Z-ADD     AWEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
      *
     C     SCEFDT        IFNE      0
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C     SRERR         IFEQ      *ON
     C*** BEGIN CHG *** 01/15/98 ***********************************************
     C******************** MOVE *ON       *IN22
     C                   MOVE      *BLANKS       SCEFDT
     C*** END CHG ***** 01/15/98 ***********************************************
     C                   ENDIF                                                  ERR = ON
     C                   ENDIF                                                  EDT = 0
      *
     C                   Z-ADD     SCEFDT        SFEDT
     C                   ADD       1             RRN
     C                   WRITE     SCR01S
     C     AWKY1         READE     PAW01                                  69
     C                   ENDDO                                                  *69 = OFF
      *
     C     RRN           IFEQ      0                                            Empty, don't
     C*** BEGIN ADD *** 01/15/98 ***********************************************
     C                   MOVE      *BLANKS       SCOPT
     C                   MOVE      *BLANKS       SCTRDT
     C                   MOVE      *BLANKS       SCAWW
     C                   MOVE      *BLANKS       SCEFDT
     C                   ADD       1             RRN
     C                   WRITE     SCR01S
     C*** END ADD ***** 01/15/98 ***********************************************
     C*** BEGIN DLT *** 01/15/98 ***********************************************
     C******************** MOVE *ON       *IN59             display
     C******************** ELSE
     C******************** MOVE *OFF      *IN59             display
     C*** END DLT ***** 01/15/98 ***********************************************
     C                   END
     C                   MOVE      *OFF          *IN83                           Update SFL
     C                   MOVE      *ON           *IN28                           Dspl again
      *
     C                   MOVEL     'SCR01'       Q2RECF
      *
     C                   ENDSR
      * ============================================================
      *   SUBROUTINE - DELET
      * ============================================================
     C     DELET         BEGSR
      *
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1TRDT
     C     SFKY          CHAIN     PAW02                              66
     C     *IN66         IFEQ      *OFF
      *
      *   User confirmation of deletion
     C                   EXFMT     SCR02
     C                   MOVE      *ON           *IN28                           Dspl again
     C**                   MOVE *ON       *IN29
     C                   MOVE      *BLANK        SCOPT
      *
     C     *INKL         IFNE      *ON
     C                   DELETE    PAW02
     C                   MOVE      *OFF          *IN81                           Next read
     C                   MOVE      *ON           *IN83                           Reload SFL
     C                   ENDIF                                                    *KV = 1
     C                   ENDIF                                                    *66 = 0
      *
     C                   ENDSR
      * ============================================================
      *   INITIALIZATION ROUTINE - *INZSR
      * ============================================================
     C     *INZSR        BEGSR
      *
      *   Define Parameter Lists
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQRTCD            1
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
     C     EMPAPI        PLIST
     C                   PARM                    EMEMP#
     C                   PARM                    EMDIV
     C                   PARM                    EMNAME
     C                   PARM      *BLANKS       XXEDN            80
      *
      *   Define fields
      *
      *
      *   Define Key Lists
      *
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CMACDT        XXTRDT
     C     *LIKE         DEFINE    CMACDT        X1TRDT
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
      *
      *   Define Key Lists
      *
      *          WMUPL - User Parameter File
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *          WMCML - Claim Master File
      *
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
      *          WMCDL - Claimant Demographics File
      *
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
      *
      *          WMAWL - Average Weekly Wage File
      *          Used to reposition subfile.
      *
     C     AWKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXTRDT
      *
     C     AWKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
      *          SMCOL - Company Master
      *
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
      *
      *          WMFNL - Fund Master File
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *          Key List for Subfile
      *
     C     SFKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    X1TRDT
     C                   KFLD                    SCTIME
      *
      *          WMEML - Claim Master File
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      *   Retrieve the User Parameter File
     C     UPKEY         CHAIN     PUP01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   ENDIF
      *
     C                   MOVE      UPCO#         XXCO#                          LOAD CMKY
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
      *
      *   Retrieve the Claim Master Record
     C     CMKY          CHAIN     PCM01                              61
     C                   Z-ADD     DBRRN         XXRRN                          FOR WMCMP
      *
     C     *IN61         IFEQ      *ON
     C                   MOVE      *ZEROS        SCSS#
     C                   MOVE      *ZEROS        SCACDT
     C                   MOVE      *ZEROS        SCFYR
     C                   MOVE      *ZEROS        SCCASE
     C                   ENDIF
      *
     C                   MOVEL     CMSS#         XXSS#                          LOAD CDKY
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
      *
      *   Retrieve the Claimant Demographics Record
     C     CDKY          CHAIN     PCD01                              62
      *
     C                   MOVEL     *BLANKS       SCCNAM
     C     CDLSN         CAT       ',':0         SCCNAM
     C     SCCNAM        CAT       CDFSN:1       SCCNAM
     C     CDMID         IFNE      *BLANK
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C     SCCNAM        CAT       '.':0         SCCNAM
     C                   END
      *
      *   Retrieve Company Record
     C     COKEY         CHAIN     PCO01                              64
      *
     C     *IN64         IFEQ      *ON
     C                   MOVE      *BLANKS       SCCONM
     C                   END
      *
      *   Retrieve Fund Master Record
     C     FNKY          CHAIN     PFN01                              65
      *
     C     *IN65         IFEQ      *ON
     C                   MOVE      *BLANKS       SCFNAM
     C                   END
      *
     C                   MOVEL     FNNAME        SCFNAM
      *
      *   Initialize Screen Fields
     C     QQCALL        IFEQ      '1'
     C                   MOVE      *OFF          *IN09
     C                   MOVE      QQCALL        SVQCAL            1
     C                   MOVE      *ON           *IN09
     C                   END
     C***** BEGIN DEL **** 04/22/99 ***********************************
     C**********           MOVELQ1PGM     SCPGM
     C**********           MOVELQ1USER    SCUSER
     C***** END DEL ****** 04/22/99 ***********************************
     C                   MOVE      PRDS(1)       SCPRDS
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   Z-ADD     XXCO#         SCCO#
     C*** BEGIN CHG *** 06/16/98 **************************************
     C******************** MOVE UPFND     SCFND
     C                   MOVE      XXFND         SCFND
     C*** END CHG ***** 06/16/98 **************************************
     C                   MOVE      *BLANKS       SCMSG
      *
     C                   TIME                    DSDTTM
     C                   MOVEL(P)  'SCR01'       Q2RECF
      *
      *  GET EXCESS REINSURANCE INFO
      *
     C     CMEXCS        IFNE      'Y'
     C     XXRRN         IFNE      *ZERO
     C                   CALL      'WA010R'
     C                   PARM                    XXRRN             9 0          FOR WMCMP
     C                   PARM                    XXFNDR           13 2
     C                   PARM                    XXEXRE            1
     C                   END
     C                   END
      *
     C     XXEXRE        IFEQ      'Y'
     C     CMEXCS        OREQ      'Y'
     C                   MOVEL(P)  XCEXRE        SCMSG1
     C                   MOVE      *ON           *IN68
     C                   ELSE
     C                   MOVE      *OFF          *IN68
     C                   END
      *
     C                   MOVE      *BLANK        SCSCL#
     C                   MOVEL(P)  CMACL#        SCSCL#
      *
      *  IF CLAIM WAS CONVERTED FROM ANOTHER SYSTEM, DISPLAY OLD CLAIM #
      *
     C     CMCVT#        IFNE      *BLANKS
     C                   MOVEL(P)  CMCVT#        SCCVT
     C                   SETON                                        64
     C                   ENDIF                                                  CMCVT#
     C*
      *  CLAIMANT SS#
     C     CMSS#         IFNE      *BLANK
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C     SS1           CAT(P)    '-':0         SCSS#
     C     SCSS#         CAT       SS2:0         SCSS#
     C     SCSS#         CAT       '-':0         SCSS#
     C     SCSS#         CAT       SS3:0         SCSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   END
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   Z-ADD     CMFYR         SCFYR                          FYR & CASE#
     C                   Z-ADD     CMCASE        SCCASE
      *  FYR-CASE
     C                   MOVE(P)   XXFYR         XAFYR             3
     C                   MOVE(P)   XXCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCS
     C     SCYRCS        CAT       XACASE:0      SCYRCS
      *
      *** Retrieve Employer information
      *
     C                   MOVE      CMEMP#        XXEMP#
     C                   MOVE      CMDIV         XXDIV
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C                   CALL      'WA005R'      EMPAPI
     C                   MOVEL     XXEDN         SCEDN
     C                   ELSE
     C                   MOVE      *BLANKS       SCEDN
     C                   ENDIF                                                  *IN60
      *
      *
      *   Load subfile
     C                   EXSR      LOAD
     C                   MOVE      *ON           *IN28
      *
     C                   ENDSR
      * ************************************************************************
** PGM DESC
   AVERAGE WEEKLY WAGE HISTORY SEARCH
