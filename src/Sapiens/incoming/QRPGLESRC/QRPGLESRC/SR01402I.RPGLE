     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SR01402I - ADDITIONAL INPUT INFO FOR WC010402A
      *****************************************************************
      *  PROGRAM DESCRIPTION:                                         *
      *    Prompts the user to enter additional information for       *
      *    endorsement WC010402A - AL Anniversary Rating Date.        *
      *                                                               *
      *  PROGRAMMER: C:113                                            *
      *  CREATED:    07/09/99               PROJECT: CM027            *
      *===============================================================*
      *  NOTE:                                                        *
      *                                                               *
      *===============================================================*
      *  REVISIONS:                                                   *
      *                                                               *
      *  05/26/00 C:90 PROJECT:WA048/22762                            *
      *                UPDATE MAINTENANCE FIELDS.                     *
      *                                                               *
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
      *===============================================================*
      ** Display File
     FSR01402F  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      ** Company Master File
     FSMCOL     IF   E           K DISK
      ** Group Master File
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDSF)
      ** Employer Master File
     FWMEML     IF   E           K DISK
      ** Endorsement workfile
     FWXFMP1    UF A E           K DISK
      *=========================================================================
      *  Data Structure and Constant definitions
      *=========================================================================

     D XXFORM          C                   CONST('WC010402  ')
      *
     D                 DS
     D  XXDATE                 1      6  0
     D  SCARMO                 1      2  0
     D  SCARDY                 3      4  0
     D  XXYEAR                 5      6  0
      *
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
      *
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      **** BEGIN ADD **** 05/26/00 ************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      **** END   ADD **** 05/26/00 ************************************
      ** External data structures
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *=========================================================================
      *  KLIST definitions
      *=========================================================================
      *
      ** Company Master file

     C     COKEY         KLIST
     C                   KFLD                    XXCO#
      *
      ** Employer Master file
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      ** Group Master file
     C     FMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFRM1
      *
      ** Endorsement workfile
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *=========================================================================
      *  PLIST definitions
      *=========================================================================
      ** Entry parameters
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
      *
      ** Date check/conversion parameters
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *=========================================================================
      *  *LIKE DEFN definitions
      *=========================================================================
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
      *=========================================================================
      *  Main
      *=========================================================================
     C                   Z-ADD     *ZEROS        XXFYR             3 0
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
     C                   MOVEL     XXFORM        XXFRM1           15
     C                   Z-ADD     99            XXYEAR
      **** BEGIN ADD **** 05/26/00 ************************************
     C                   TIME                    DSDTTM
      *
     C                   MOVE      *OFF          *IN20
      **** END   ADD **** 05/26/00 ************************************
      *
     C                   Z-ADD     XXFND         SCFND
     C                   Z-ADD     XXEMP#        SCEMP#
     C                   Z-ADD     XXDIV         SCDIV
     C                   Z-ADD     XXFYR         SCFYR
      *
      ** Load the record format information for the first time
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL(P)  'SCR01'       Q2RECF
      *
      ** Get the Company record
     C     COKEY         CHAIN     PCO01                              60
      *
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
      ** Get the Group record
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL(P)  FNNAME        SCFNAM
      *
      ** Get the Employer record
     C     EMKY          CHAIN     PEM01                              60
     C                   MOVEL(P)  EMNAME        SCENAM
      *
     C                   MOVE      Q1USER        SCUSER
      *
      ** Load the screen fields
     C                   EXSR      MVDBSC
      **************************************************************************
      *  Display all formats
      **************************************************************************
     C     STRT          TAG
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
      **************************************************************************
      *  Process all Function keys
      **************************************************************************
     C     *IN11         IFEQ      *ON
      *
      ** F3=Exit
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   END
      *
      ** F12=Cancel
     C     *INKL         IFEQ      *ON
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   GOTO      TERM
     C                   ENDIF                                                  SCR01
      *
     C                   ENDIF                                                  INKL=ON
      *
     C                   ENDIF                                                  IN11=ON
      **************************************************************************
      *  End of Function key processing
      **************************************************************************
      *
      ** Goto the appropriate screen
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
      **************************************************************************
      * Process Screen 01
      **************************************************************************
     C     SCR01T        TAG
      *
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C     SRERR         IFEQ      *ON
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
      *
      ** All edits passed for screen 1, write or update record
     C   60              CLEAR                   PFM01
      *
     C                   EXSR      MVSCDB
      *
     C   60              WRITE     PFM01
     C  N60              UPDATE    PFM01
      **************************************************************************
      * End of program processing
      **************************************************************************
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *=========================================================================
      * MVDBSC - Move database fields to the screen fields
      *=========================================================================
     C     MVDBSC        BEGSR
      *
      ** Get the Endorsement workfile record
     C     FMKY          CHAIN     PFM01                              60
      *
     C     *IN60         IFEQ      *OFF
      **** BEGIN ADD **** 05/26/00 ************************************
     C                   MOVE      *ON           *IN20
      **** END   ADD **** 05/26/00 ************************************
      *
      ** SCR01 fields
     C                   Z-ADD     F3ANMO        SCARMO
     C                   Z-ADD     F3ANDA        SCARDY
     C                   ELSE
     C                   Z-ADD     *ZEROS        SCARMO
     C                   Z-ADD     *ZEROS        SCARDY
     C                   ENDIF
      *
     C                   ENDSR                                                  MVDBSC
      *=========================================================================
      *  MVSCDB - Move screen fields to database fields
      *=========================================================================
     C     MVSCDB        BEGSR
      *
     C                   Z-ADD     XXCO#         F3CO#
     C                   Z-ADD     XXFND         F3FND
     C                   Z-ADD     XXFYR         F3FYR
     C                   Z-ADD     XXEMP#        F3EMP#
     C                   Z-ADD     XXDIV         F3DIV
     C                   MOVEL     XXFORM        F3FORM
      **** BEGIN ADD **** 05/26/00 ************************************
     C                   MOVEL     Q1USER        F3USER
     C                   MOVEL     Q1PGM         F3PGM
     C                   Z-ADD     DSDATE        F3DT
     C                   Z-ADD     DSTIME        F3TIME
     C*
     C  N20              MOVEL     Q1USER        F3IUSR
     C  N20              MOVEL     Q1PGM         F3IPGM
     C  N20              Z-ADD     DSDATE        F3IDT
      **** END   ADD **** 05/26/00 ************************************
      *
      ** SCR01 fields
     C                   Z-ADD     SCARMO        F3ANMO
     C                   Z-ADD     SCARDY        F3ANDA
      *
     C                   ENDSR                                                  MBSCDB
