     *===============================================================*
     *  (C) Copyright Progressive Data Solutions, Inc.,  1999        *
     *===============================================================*
     H/TITLE SR21307I - ADDITIONAL INPUT INFO FOR WC210307
     *===============================================================*
     * PROGRAM DESCRIPTION:                                          *
     *   This program prompts the user to enter additional info for  *
     *   endorsement WC210307 - MI Specific Person Exclusion         *
     *   Endorsement                                                 *
     *                                                               *
     * Created   : 08/25/99                                          *
     * Project   : 96WCM                                             *
     * Programmer: C:113                                             *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
      *  05/29/01 C:90 PROJECT:CM027                                  *
      *                Load proper fields for the document.           *
      *                                                               *
      *  05/26/00 C:90 PROJECT:WA048/22762                            *
      *                UPDATE MAINTENANCE FIELDS.                     *
      *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS            *
     * ------------------------------------------------------------- *
     *===============================================================*
     * NOTES:                                                        *
     *                                                               *
     *  File WXFMP1 stores data in generic data fields.  Each        *
     *  customized form uses these fields differently.  This is a    *
     *  list of the available generic field formats.                 *
     *                                                               *
     *  F3A521 - F3A52A  AMOUNT    5.2 PACKED                        *
     *  F3A541 - F3A54A  AMOUNT    5.4 PACKED                        *
     *  F3A701 - F3A70A  AMOUNT    7.0 PACKED                        *
     *  F3A921 - F3A92A  AMOUNT    9.2 PACKED                        *
     *  F3AM01 - F3AM10  AMOUNT   11.2 PACKED                        *
     *  F3FL01 - F3FL20  FLAG        1 CHARACTER                     *
     *  F3D601 - F3D616  DATE      6.0 PACKED                        *
     *  F3DT01 - F3DT16  DATE      8.0 ZONED                         *
     *  F3C101 - F3C116  GENERIC    10 CHARACTER                     *
     *  F3C201 - F3C216  GENERIC    20 CHARACTER                     *
     *  F3C301 - F3C316  GENERIC    30 CHARACTER                     *
     *  F3C401 - F3C442  GENERIC    40 CHARACTER                     *
     *  F3NM01 - F3NM14  NAME       46 CHARACTER                     *
     *===============================================================*
     *=========================================================================
     *  File Definitions
     *=========================================================================
     ** Display File
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FSR21307F  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     ** Company Master File
     FSMCOL     IF   E           K DISK
     ** Employer Master File
     FWMEML     IF   E           K DISK
     ** Group Master File
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDSF)
     ** Endorsement Workfile
     FWXFMP1    UF A E           K DISK
     *=========================================================================
     *  Array & Table Definitions
     *=========================================================================
      *
      *
     *=========================================================================
     *  Data Structure & Constant Definitions
     *=========================================================================
     D XXFORM          C                   CONST('WC210307  ')
      *
     ** Employer Master RRN
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
      *
     ** Screen record format
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      **** BEGIN ADD **** 05/26/00 ************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      **** END   ADD **** 05/26/00 ************************************
      *
     ** Name field structure
     D**** BEGIN CHG **** 05/29/01 ************************************
     D****************DSNAME      DS                            460
     D DSNAME          DS           230
     D  SCNM01                 1     46
     D  SCNM02                47     92
     D  SCNM03                93    138
     D  SCNM04               139    184
     D  SCNM05               185    230
     D************************************* 231 276 SCNM06
     D************************************* 277 322 SCNM07
     D************************************* 323 368 SCNM08
     D************************************* 369 414 SCNM09
     D************************************* 415 460 SCNM10
     D**** END   CHG **** 05/29/01 ************************************
      *
     ** External data structures
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     *=========================================================================
     *  KLIST Definitions
     *=========================================================================
     ** Company Master file
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
      *
     ** Employer Master file
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
     ** Employer Endorsement file
     C     FMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFRM1
     ** Group Master file
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     *=========================================================================
     *  PLIST Definitions
     *=========================================================================
     ** Entry parameters
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
      *
     ** Date check and conversion parameters
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
      *
     ** Center field parameters
     C     COPL          PLIST
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     *=========================================================================
     *  *LIKE DEFN Definitions
     *=========================================================================
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    F3FYR         XXFYR
     C     *LIKE         DEFINE    F3FORM        XXFRM1
     *=========================================================================
     *  Main
     *=========================================================================
      *
     ** Initializations
     C                   Z-ADD     *ZEROS        XXFYR
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
     C                   Z-ADD     XXFND         SCFND
     C                   Z-ADD     XXFYR         SCFYR
     C                   MOVEL     XXFORM        XXFRM1
      **** BEGIN ADD **** 05/26/00 ************************************
     C                   TIME                    DSDTTM
      *
     C                   MOVE      *OFF          *IN20
      **** END   ADD **** 05/26/00 ************************************
      *
     ** Load the record format info for the first time
     C                   MOVEL(P)  'SCR01'       Q2RECF
      *
     ** Get the Company record
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'    COPL
     C                   MOVEL     SRDFLD        SCCONM
      *
     ** Get the Group record
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL(P)  FNNAME        SCFNAM
      *
     ** Get the Employer Master record
     C     EMKY          CHAIN     PEM01                              60
      *
     ** Compress Employer/Div/Name
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
      *
     C                   MOVEL     XXEDN         SCEDN
      *
     C                   MOVE      Q1USER        SCUSER
      *
     ** Move database fields to screen
     C                   EXSR      MVDBSC
     **************************************************************************
     C     STRT          TAG
     **************************************************************************
      *
     ** Reset error indicator
     C     *IN90         IFEQ      *ON
     C                   SETOFF                                       90
     C                   ENDIF                                                  *IN90 = *ON
      *
     ** Display the screen
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
      *
     ** Check for function key press
     C     *IN11         IFEQ      *ON
     C                   EXSR      FUNCP
     C                   ENDIF                                                  *IN11 = *ON
      *
     ** Goto the appropriate screen
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     **************************************************************************
     C     SCR01T        TAG
     **************************************************************************
      *
     ** Process request screen
     C                   EXSR      ERRCK
      *
     ** If there is an error, go back to start
     C     *IN90         CABEQ     *ON           STRT
      *
     ** All edits passed
     C                   EXSR      MVSCDB
      *
     ** End the program
     **************************************************************************
     C     TERM          TAG
     **************************************************************************
     C                   MOVE      *ON           *INLR
     *=========================================================================
     *  ERRCK - Subroutine to check for errors in the screen fields
     *=========================================================================
     C     ERRCK         BEGSR
      *
     C                   MOVE      *OFF          *IN90
     C                   MOVEA     '00000000'    *IN(21)
     C                   MOVEA     '00'          *IN(29)
      *
     C  N90              SELECT
     C     SCFL01        WHENEQ    'Y'
     C     SCNM01        IFEQ      *BLANKS
     C                   SETON                                        32  90
     C                   ENDIF
      *
     C     SCFL01        WHENEQ    'N'
     C                   MOVE      *BLANK        T                 1
     C                   Z-ADD     *ZERO         R                 3 0
     C     ' '           CHECK     DSNAME        R
      *
     C     R             IFNE      *ZERO
     C     R             DIV       46            R
     C                   ADD       21            R
     C                   MOVE      *ON           *IN(R)
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *
     C                   OTHER
     C                   SETON                                        31  90
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  ERRCK
     *=========================================================================
     *  MVDBSC - Subroutine to move the database fields to the screen
     *=========================================================================
     C     MVDBSC        BEGSR
      *
     ** Chain to the Employer Attached Endorsement file
     C     FMKY1         CHAIN     PFM01                              60
     C     *IN60         IFEQ      *OFF
      **** BEGIN ADD **** 05/26/00 ************************************
     C                   MOVE      *ON           *IN20
      **** END   ADD **** 05/26/00 ************************************
      *
     ** Move custom fields to the screen
     C                   MOVE      F3FL01        SCFL01
      **** BEGIN ADD **** 05/29/01 ************************************
     C                   MOVE      F3FL04        SCPEXC
     C                   SELECT
     C     F3FL04        WHENEQ    'P'
      **** END   ADD **** 05/29/01 ************************************
     C                   MOVEL     F3NM01        SCNM01
     C                   MOVEL     F3NM02        SCNM02
     C                   MOVEL     F3NM03        SCNM03
     C                   MOVEL     F3NM04        SCNM04
     C                   MOVEL     F3NM05        SCNM05
      **** BEGIN ADD **** 05/29/01 ************************************
     C*
     C     F3FL04        WHENEQ    'O'
     C                   MOVEL     F3NM06        SCNM01
     C                   MOVEL     F3NM07        SCNM02
     C                   MOVEL     F3NM08        SCNM03
     C                   MOVEL     F3NM09        SCNM04
     C                   MOVEL     F3NM10        SCNM05
     C*
     C     F3FL04        WHENEQ    'O'
     C                   MOVEL     F3NM11        SCNM01
     C                   MOVEL     F3NM12        SCNM02
     C                   MOVEL     F3NM13        SCNM03
     C                   MOVEL     F3NM14        SCNM04
     C                   MOVEL     F3C401        SCNM05
     C*
     C                   ENDSL
      **** END   ADD **** 05/29/01 ************************************
      *
     C                   ELSE
      *
     C                   MOVE      'Y'           SCFL01
     C                   MOVE      *BLANKS       DSNAME
      *
     C                   ENDIF                                                  *IN60 = *BLANKS
      *
     C                   ENDSR                                                  MVDBSC
     *=========================================================================
     *  MVSCDB - Subroutine to move the screen fields to the database
     *=========================================================================
     C     MVSCDB        BEGSR
      *
     C     FMKY1         CHAIN     PFM01                              60
      *
     C                   Z-ADD     XXCO#         F3CO#
     C                   Z-ADD     XXFND         F3FND
     C                   Z-ADD     XXFYR         F3FYR
     C                   Z-ADD     XXEMP#        F3EMP#
     C                   Z-ADD     XXDIV         F3DIV
     C                   MOVEL     XXFORM        F3FORM
      *
     ** Move the custom fields to the file
     C     SCFL01        IFEQ      'Y'
     C                   MOVE      'X'           F3FL02
     C                   MOVE      *BLANK        F3FL03
     C                   ELSE
     C                   MOVE      *BLANK        F3FL02
     C                   MOVE      'X'           F3FL03
     C                   ENDIF                                                  SCFL01 = Y
     C**** BEGIN ADD **** 05/29/01 ************************************
     C                   MOVE      SCFL01        F3FL01
     C                   MOVE      SCPEXC        F3FL04
     C**** END   ADD **** 05/29/01 ************************************
      *
     C**** BEGIN ADD **** 05/29/01 ************************************
     C                   SELECT
     C*
     C     SCPEXC        WHENEQ    'P'
     C**** END   ADD **** 05/29/01 ************************************
     C                   MOVEL     SCNM01        F3NM01
     C                   MOVEL     SCNM02        F3NM02
     C                   MOVEL     SCNM03        F3NM03
     C                   MOVEL     SCNM04        F3NM04
     C                   MOVEL     SCNM05        F3NM05
     C**** BEGIN ADD **** 05/29/01 ************************************
     C                   MOVEL     *BLANKS       F3NM06
     C                   MOVEL     *BLANKS       F3NM07
     C                   MOVEL     *BLANKS       F3NM08
     C                   MOVEL     *BLANKS       F3NM09
     C                   MOVEL     *BLANKS       F3NM10
     C                   MOVEL     *BLANKS       F3NM11
     C                   MOVEL     *BLANKS       F3NM12
     C                   MOVEL     *BLANKS       F3NM13
     C                   MOVEL     *BLANKS       F3NM14
     C                   MOVEL     *BLANKS       F3C401
     C**** END   ADD **** 05/29/01 ************************************
     C**** BEGIN DEL **** 05/29/01 ************************************
     C******************** MOVELSCNM06    F3NM06
     C******************** MOVELSCNM07    F3NM07
     C******************** MOVELSCNM08    F3NM08
     C******************** MOVELSCNM09    F3NM09
     C******************** MOVELSCNM10    F3NM10
     C**** END   DEL **** 05/29/01 ************************************
     C**** BEGIN ADD **** 05/29/01 ************************************
     C*
     C     SCPEXC        WHENEQ    'O'
     C                   MOVEL     SCNM01        F3NM06
     C                   MOVEL     SCNM02        F3NM07
     C                   MOVEL     SCNM03        F3NM08
     C                   MOVEL     SCNM04        F3NM09
     C                   MOVEL     SCNM05        F3NM10
     C                   MOVEL     *BLANKS       F3NM01
     C                   MOVEL     *BLANKS       F3NM02
     C                   MOVEL     *BLANKS       F3NM03
     C                   MOVEL     *BLANKS       F3NM04
     C                   MOVEL     *BLANKS       F3NM05
     C                   MOVEL     *BLANKS       F3NM11
     C                   MOVEL     *BLANKS       F3NM12
     C                   MOVEL     *BLANKS       F3NM13
     C                   MOVEL     *BLANKS       F3NM14
     C                   MOVEL     *BLANKS       F3C401
     C*
     C     SCPEXC        WHENEQ    ' '
     C                   MOVEL     SCNM01        F3NM11
     C                   MOVEL     SCNM02        F3NM12
     C                   MOVEL     SCNM03        F3NM13
     C                   MOVEL     SCNM04        F3NM14
     C                   MOVEL     SCNM05        F3C401
     C                   MOVEL     *BLANKS       F3NM01
     C                   MOVEL     *BLANKS       F3NM02
     C                   MOVEL     *BLANKS       F3NM03
     C                   MOVEL     *BLANKS       F3NM04
     C                   MOVEL     *BLANKS       F3NM05
     C                   MOVEL     *BLANKS       F3NM06
     C                   MOVEL     *BLANKS       F3NM07
     C                   MOVEL     *BLANKS       F3NM08
     C                   MOVEL     *BLANKS       F3NM09
     C                   MOVEL     *BLANKS       F3NM10
     C*
     C                   ENDSL
     C**** END   ADD **** 05/29/01 ************************************
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
     C   60              WRITE     PFM01
     C  N60              UPDATE    PFM01
      *
     C                   ENDSR                                                  MVSCDB
     *=========================================================================
     *  FUNCP - Subroutine to process function keys
     *=========================================================================
     C     FUNCP         BEGSR
      *
     ** F3=Quit
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   MOVE      'F'           QQCALL
     C                   GOTO      TERM
     C                   ENDIF
      *
     ** F12=Cancel
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   MOVE      'F'           QQCALL
     C                   GOTO      TERM
     C                   ENDIF                                                  INKL=ON
      *
     C                   ENDSR                                                  FUNCP
