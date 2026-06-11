     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7264R - WORK WITH CLAIM TRANSFERS
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY TRANSFERS DETAIL     *
     H*                INFO BASED ON THE SELECTION FROM THE WORK WITH.*
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W7264R                                                     *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 05/02/00  PROGRAMMER: C:53  PROJECT: WC836          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H*****************************************************************
     H*
     FW7264F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWACTP     UF A E             DISK
     F                                     RENAME(PCT01:DBRCDF)
     FWMFNL     IF   E           K DISK
     FWTGTL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
      ***  Begin Add  ***  06/09/20  ****************************
      /copy sPrMask
      /copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Claim Transfers')
     D COMAPI          C                   CONST('WA001R')
     D CHGNUM          C                   CONST('SA020R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     CHGPL         PLIST
     C                   PARM                    ##NUM            80
     C                   PARM                    ##MASK           10
     C                   PARM      *ZEROS        ##DEC             3 0
     C                   PARM      *BLANK        ##THOU            1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKY          KLIST
     C                   KFLD                    CTCO#
     C                   KFLD                    CTFND
      *
      *  SYSTEM TABLE VALUE FILE
      *
     C     GTKY          KLIST
     C                   KFLD                    CTCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
      *
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*** PICK UP FUND NAME
     C*
     C     FNKY          CHAIN     PFN01                              99
     C  N99              MOVEL(P)  FNNAME        SCNAME
     C                   MOVE      CTFND         SCFND
     C*
     C                   MOVE      CTXFYR        SCXFYR
     C                   MOVE      CTXCAS        SCXCAS
     C                   MOVE      CTXEMP        SCXEMP
     C                   MOVE      CTXDIV        SCXDIV
     C                   MOVE      CTXCLS        SCXCLS
     C*
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              MOVEL(P)  CTXSS#        ##NUM
     C*****              MOVEL(P)  'SSN'         ##MASK
     C*****              CALL      CHGNUM        CHGPL
     C*****              MOVEL     ##NUM         SCXSS#
      /Free
       scXSS# = DisplaySS#(MaskData('SSN':'CTXSS#':CTXSS#));
      /End-free
      *****  End Chg  ***  06/09/20  **********************
     C*
     C                   Z-ADD     CTXACD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         ##NUM
     C                   MOVEL(P)  'DATE'        ##MASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     ##NUM         SCXACD
     C*
     C                   MOVE      CTFYR         SCFYR
     C                   MOVE      CTCASE        SCCASE
     C                   MOVE      CTEMP#        SCEMP#
     C                   MOVE      CTDIV         SCDIV
     C                   MOVE      CTCLAS        SCCLAS
     C*
      *****  Beg Chg  ***  06/09/20  **********************
     C*****              MOVEL(P)  CTSS#         ##NUM
     C*****              MOVEL(P)  'SSN'         ##MASK
     C*****              CALL      CHGNUM        CHGPL
     C*****              MOVEL     ##NUM         SCSS#
      /Free
       SCSS# = DisplaySS#(MaskData('SSN':'CTSS#':CTSS#));
      /End-free
      *****  End Chg  ***  06/09/20  **********************
     C*
     C                   Z-ADD     CTACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         ##NUM
     C                   MOVEL(P)  'DATE'        ##MASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     ##NUM         SCACDT
     C*
     C                   Z-ADD     CTAMT         SCAMT
     C                   MOVEL(P)  CTREQ         SCREQ
     C                   MOVEL(P)  CTDLTR        SCDLTR
     C                   MOVEL(P)  CTUSER        SCUSER
     C                   MOVEL(P)  CTPGM         SCPGM
     C*
     C                   MOVEL(P)  'DLTR'        XXTBID
     C                   MOVEL(P)  CTDLTR        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCDLTD
     C   99              MOVE(P)   *BLANKS       SCDLTD
     C*
     C                   Z-ADD     CTDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         ##NUM
     C                   MOVEL(P)  'DATE'        ##MASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     ##NUM         SCDT
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      WACTP
     C                   ENDSR
