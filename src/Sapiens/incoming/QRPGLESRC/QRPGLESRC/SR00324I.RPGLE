     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     *===============================================================*
     *===============================================================*
     H/TITLE SR00324I - ADDITIONAL INPUT INFO FOR WC000324
     *===============================================================*
     * PROGRAM DESCRIPTION:                                          *
     *   This program prompts the user to enter additional info for  *
     *   endorsement WC000324 - Other States Endorsement.            *
     *                                                               *
     * Created   : 08/04/99                                          *
     * Project   : CM027                                             *
     * Programmer: C:113                                             *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
      *  05/23/00 C:90 PROJECT:WA048/22762                            *
      *                UPDATE MAINTENANCE FIELDS.                     *
      *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS            *
     * ------------------------------------------------------------- *
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
     FSR00324F  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     ** System State Table file
     FSTABL     IF   E           K DISK
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
     ** Default verbiage array

     D VERB            S             46    DIM(2) CTDATA PERRCD(1)
     *=========================================================================
     *  Data Structure & Constant Definitions
     *=========================================================================
     D XXFORM          C                   CONST('WC000324  ')
      *
     ** Employer Master RRN
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
      *
     ** Screen record format
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      *
     ** States data structure
     D XXNM01          DS
     D  SCST01                 1      2
     D  SCST02                 4      5
     D  SCST03                 7      8
     D  SCST04                10     11
     D  SCST05                13     14
     D  SCST06                16     17
     D  SCST07                19     20
     D  SCST08                22     23
     D  SCST09                25     26
     D  SCST10                28     29
     D  SCST11                31     32
     D  SCST12                34     35
     D  SCST13                37     38
     D  SCST14                40     41
     D  SCST15                43     44
      **** BEGIN ADD **** 05/23/00 ************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      **** END   ADD **** 05/23/00 ************************************
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
      *
     ** System State Table file
     C     STKY          KLIST
     C                   KFLD                    XXKEY            10
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
      **** BEGIN ADD **** 05/23/00 ************************************
     C                   TIME                    DSDTTM
      *
     C                   MOVE      *OFF          *IN20
      **** END   ADD **** 05/23/00 ************************************
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
     ** Verbiage field cannot be blank
     C     *IN90         IFEQ      *OFF
     C     SCVER1        ANDEQ     *BLANK
     C                   SETON                                        30  90
     C                   ENDIF
      *
     ** Check each state field for a valid state
     C     *IN90         IFEQ      *OFF
     C     SCST01        ANDNE     *BLANKS
     C                   MOVEL     SCST01        XXKEY
     C     STKY          CHAIN     PST01                              60
     C   60              SETON                                        31  90
     C                   ENDIF                                                  *IN90 = *OFF
      *
     C     *IN90         IFEQ      *OFF
     C     SCST02        ANDNE     *BLANKS
     C                   MOVEL     SCST02        XXKEY
     C     STKY          CHAIN     PST01                              60
     C   60              SETON                                        32  90
     C                   ENDIF                                                  *IN90 = *OFF
      *
     C     *IN90         IFEQ      *OFF
     C     SCST03        ANDNE     *BLANKS
     C                   MOVEL     SCST03        XXKEY
     C     STKY          CHAIN     PST01                              60
     C   60              SETON                                        33  90
     C                   ENDIF                                                  *IN90 = *OFF
      *
     C     *IN90         IFEQ      *OFF
     C     SCST04        ANDNE     *BLANKS
     C                   MOVEL     SCST04        XXKEY
     C     STKY          CHAIN     PST01                              60
     C   60              SETON                                        34  90
     C                   ENDIF                                                  *IN90 = *OFF
      *
     C     *IN90         IFEQ      *OFF
     C     SCST05        ANDNE     *BLANKS
     C                   MOVEL     SCST05        XXKEY
     C     STKY          CHAIN     PST01                              60
     C   60              SETON                                        35  90
     C                   ENDIF                                                  *IN90 = *OFF
      *
     C     *IN90         IFEQ      *OFF
     C     SCST06        ANDNE     *BLANKS
     C                   MOVEL     SCST06        XXKEY
     C     STKY          CHAIN     PST01                              60
     C   60              SETON                                        36  90
     C                   ENDIF                                                  *IN90 = *OFF
      *
     C     *IN90         IFEQ      *OFF
     C     SCST07        ANDNE     *BLANKS
     C                   MOVEL     SCST07        XXKEY
     C     STKY          CHAIN     PST01                              60
     C   60              SETON                                        37  90
     C                   ENDIF                                                  *IN90 = *OFF
      *
     C     *IN90         IFEQ      *OFF
     C     SCST08        ANDNE     *BLANKS
     C                   MOVEL     SCST08        XXKEY
     C     STKY          CHAIN     PST01                              60
     C   60              SETON                                        38  90
     C                   ENDIF                                                  *IN90 = *OFF
      *
     C     *IN90         IFEQ      *OFF
     C     SCST09        ANDNE     *BLANKS
     C                   MOVEL     SCST09        XXKEY
     C     STKY          CHAIN     PST01                              60
     C   60              SETON                                        39  90
     C                   ENDIF                                                  *IN90 = *OFF
      *
     C     *IN90         IFEQ      *OFF
     C     SCST10        ANDNE     *BLANKS
     C                   MOVEL     SCST10        XXKEY
     C     STKY          CHAIN     PST01                              60
     C   60              SETON                                        40  90
     C                   ENDIF                                                  *IN90 = *OFF
      *
     C     *IN90         IFEQ      *OFF
     C     SCST11        ANDNE     *BLANKS
     C                   MOVEL     SCST11        XXKEY
     C     STKY          CHAIN     PST01                              60
     C   60              SETON                                        41  90
     C                   ENDIF                                                  *IN90 = *OFF
      *
     C     *IN90         IFEQ      *OFF
     C     SCST12        ANDNE     *BLANKS
     C                   MOVEL     SCST12        XXKEY
     C     STKY          CHAIN     PST01                              60
     C   60              SETON                                        42  90
     C                   ENDIF                                                  *IN90 = *OFF
      *
     C     *IN90         IFEQ      *OFF
     C     SCST13        ANDNE     *BLANKS
     C                   MOVEL     SCST13        XXKEY
     C     STKY          CHAIN     PST01                              60
     C   60              SETON                                        43  90
     C                   ENDIF                                                  *IN90 = *OFF
      *
     C     *IN90         IFEQ      *OFF
     C     SCST14        ANDNE     *BLANKS
     C                   MOVEL     SCST14        XXKEY
     C     STKY          CHAIN     PST01                              60
     C   60              SETON                                        44  90
     C                   ENDIF                                                  *IN90 = *OFF
      *
     C     *IN90         IFEQ      *OFF
     C     SCST15        ANDNE     *BLANKS
     C                   MOVEL     SCST15        XXKEY
     C     STKY          CHAIN     PST01                              60
     C   60              SETON                                        45  90
     C                   ENDIF                                                  *IN90 = *OFF
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
      **** BEGIN ADD **** 05/23/00 ************************************
     C                   MOVE      *ON           *IN20
      **** END   ADD **** 05/23/00 ************************************
     ** Move custom fields to the screen
     C                   MOVEL     F3NM01        XXNM01
     C                   MOVEL     F3NM02        SCVER1
     C                   MOVEL     F3NM03        SCVER2
     C                   ELSE
     C                   MOVEL     VERB(1)       SCVER1
     C                   MOVEL     VERB(2)       SCVER2
     C                   ENDIF                                                  F3NM02 = *BLANKS
      *
     C                   ENDSR                                                  MVDBSC
     *=========================================================================
     *  MVSCDB - Subroutine to move the screen fields to the database
     *=========================================================================
     C     MVSCDB        BEGSR
      *
     C     FMKY1         CHAIN     PFM01                              60
      *
     C                   MOVEL     XXNM01        F3NM01
     C                   Z-ADD     XXCO#         F3CO#
     C                   Z-ADD     XXFND         F3FND
     C                   Z-ADD     XXFYR         F3FYR
     C                   Z-ADD     XXEMP#        F3EMP#
     C                   Z-ADD     XXDIV         F3DIV
     C                   MOVEL     XXFORM        F3FORM
      *
     ** Move the custom fields to the file
     C                   MOVEL     SCVER1        F3NM02
     C                   MOVEL     SCVER2        F3NM03
      **** BEGIN ADD **** 05/23/00 ************************************
     C                   MOVEL     Q1USER        F3USER
     C                   MOVEL     Q1PGM         F3PGM
     C                   Z-ADD     DSDATE        F3DT
     C                   Z-ADD     DSTIME        F3TIME
     C*
     C  N20              MOVEL     Q1USER        F3IUSR
     C  N20              MOVEL     Q1PGM         F3IPGM
     C  N20              Z-ADD     DSDATE        F3IDT
      **** END   ADD **** 05/23/00 ************************************
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
** VERB
All states except for those listed in Item 3.A.
and those listed below:
