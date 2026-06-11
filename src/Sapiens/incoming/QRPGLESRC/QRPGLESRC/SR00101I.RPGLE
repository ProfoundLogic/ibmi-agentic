     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SR00101I - ADDITIONAL INPUT INFO FOR WC000101A
      *===============================================================*
      *  PROGRAM DESCRIPTION:                                         *
      *    This program prompts the user to enter additional info for *
      *    endorsement WC000101A - Defense Base Act Coverage.         *
      *                                                               *
      *  PROGRAMMER: C:113                                            *
      *  CREATED:    06/28/99               PROJECT: CM027            *
      *===============================================================*
      *  REVISIONS:                                                   *
      *                                                               *
      * 05/22/00  C:90 PROJECT:WA048/22762                            *
      *                UPDATE MAINTENANCE FIELDS.                     *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS            *
      * ------------------------------------------------------------- *
      *===============================================================*
      *  NOTE:                                                        *
      *                                                               *
      *  File WXFMP1 stores data in generic data fields.  Each        *
      *  customized form uses these fields differently.  This is a    *
      *  list of the available generic field formats.                 *
      *                                                               *
      *  F3A521 - F3A52A  AMOUNT    5.2 PACKED                        *
      *  F3A541 - F3A54A  AMOUNT    5.4 PACKED                        *
      *  F3A701 - F3A70A  AMOUNT    7.0 PACKED                        *
      *  F3A921 - F3A92A  AMOUNT    9.2 PACKED                        *
      *  F3AM01 - F3AM10  AMOUNT   11.2 PACKED                        *
      *  F3FL01 - F3FL20  FLAG        1 CHARACTER                     *
      *  F3D601 - F3D616  DATE      6.0 PACKED                        *
      *  F3DT01 - F3DT16  DATE      8.0 ZONED                         *
      *  F3C101 - F3C116  GENERIC    10 CHARACTER                     *
      *  F3C201 - F3C216  GENERIC    20 CHARACTER                     *
      *  F3C301 - F3C316  GENERIC    30 CHARACTER                     *
      *  F3C401 - F3C442  GENERIC    40 CHARACTER                     *
      *  F3NM01 - F3NM14  NAME       46 CHARACTER                     *
      *===============================================================*
      ** Display File
     FSR00101F  CF   E             WORKSTN
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
      ** Endorsement Workfile
     FWXFMP1    UF A E           K DISK
      *=========================================================================
      *  Data Structur & Constant Definitions
      *=========================================================================

     D XXFORM          C                   CONST('WC000101A ')
      *
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
      *
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      **** BEGIN ADD **** 05/22/00 ************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      **** END   ADD **** 05/22/00 ************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *=========================================================================
      *  KLIST Definitions
      *=========================================================================
      ** Company Master file

     C     COKEY         KLIST
     C                   KFLD                    XXCO#
      *
      ** Group Master file
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      ** Employer Master file
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      ** Employer Endorsement file
     C     FMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFRM1
      *=========================================================================
      *  PLIST Definitions
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
      ** Date check and conversion parameters
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
     C     COPL          PLIST
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
      *=========================================================================
      *  *LIKE DEFN Definitions
      *=========================================================================
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    F3FYR         XXFYR
     C     *LIKE         DEFINE    F3FORM        XXFRM1
      *=========================================================================
      *  Main
      *=========================================================================
     C                   Z-ADD     *ZEROS        XXFYR
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
     C                   Z-ADD     XXFND         SCFND
     C                   Z-ADD     XXFYR         SCFYR
     C                   MOVEL     XXFORM        XXFRM1
      **** BEGIN ADD **** 05/22/00 ************************************
     C                   TIME                    DSDTTM
      *
     C                   MOVE      *OFF          *IN20
      **** END   ADD **** 05/22/00 ************************************
      *
      ** Load the record format info for the first time
     C                   MOVEL(P)  'SCR01'       Q2RECF
      *
      ** Get the Company record
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'    COPL
     C                   MOVEL     SRDFLD        SCCONM
      *
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL(P)  FNNAME        SCFNAM
      *
     C     EMKY          CHAIN     PEM01                              60
      *
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
      ** Get the endorsement workfile
     C     FMKY1         CHAIN     PFM01                              60
     C     *IN60         IFEQ      *OFF
      **** BEGIN ADD **** 05/22/00 ************************************
     C                   MOVE      *ON           *IN20
      **** END   ADD **** 05/22/00 ************************************
      *
      ** Move custom fields to the screen
     C                   MOVEL     F3NM01        SCDOW1
     C                   MOVEL     F3NM02        SCDOW2
     C                   MOVEL     F3NM03        SCDOW3
     C                   MOVEL     F3NM04        SCDOW4
     C                   MOVEL     F3NM05        SCDOW5
     C                   MOVEL     F3NM06        SCDOW6
     C                   MOVEL     F3NM07        SCDOW7
     C                   MOVEL     F3NM08        SCDOW8
     C                   MOVEL     F3NM09        SCDOW9
     C                   MOVEL     F3NM10        SCDW10
      *
     C                   ENDIF                                                  *IN60 = *OFF
      **************************************************************************
      *  DISPLAY ALL FORMATS
      **************************************************************************
     C     STRT          TAG
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
      **************************************************************************
      *  PROCESS ALL COMMAND KEYS
      **************************************************************************
     C     *IN11         IFEQ      *ON
      *
      ** F3=Quit
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   MOVE      'F'           QQCALL
     C                   GOTO      TERM
     C                   ENDIF
      *
      ** F12=Cancel
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   MOVE      'F'           QQCALL
     C                   GOTO      TERM
     C                   ENDIF                                                  INKL=ON
      *
     C                   ENDIF                                                  IN11=ON
      *
      ** Goto the appropriate screen
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
      *
      ** Process request screen
     C     SCR01T        TAG
      *
     C                   EXSR      MVSCDB
      *
      ** End the program
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *=========================================================================
      *  MVSCDB - Subroutine to move the screen fields to the database
      *=========================================================================
     C     MVSCDB        BEGSR
      *
     C   60              CLEAR                   PFM01
     C                   Z-ADD     XXCO#         F3CO#
     C                   Z-ADD     XXFND         F3FND
     C                   Z-ADD     XXFYR         F3FYR
     C                   Z-ADD     XXEMP#        F3EMP#
     C                   Z-ADD     XXDIV         F3DIV
     C                   MOVEL     XXFORM        F3FORM
      *
      ** Move the custom fields to the file
     C                   MOVEL     SCDOW1        F3NM01
     C                   MOVEL     SCDOW2        F3NM02
     C                   MOVEL     SCDOW3        F3NM03
     C                   MOVEL     SCDOW4        F3NM04
     C                   MOVEL     SCDOW5        F3NM05
     C                   MOVEL     SCDOW6        F3NM06
     C                   MOVEL     SCDOW7        F3NM07
     C                   MOVEL     SCDOW8        F3NM08
     C                   MOVEL     SCDOW9        F3NM09
     C                   MOVEL     SCDW10        F3NM10
      **** BEGIN ADD **** 05/22/00 ************************************
     C                   MOVEL     Q1USER        F3USER
     C                   MOVEL     Q1PGM         F3PGM
     C                   Z-ADD     DSDATE        F3DT
     C                   Z-ADD     DSTIME        F3TIME
     C*
     C  N20              MOVEL     Q1USER        F3IUSR
     C  N20              MOVEL     Q1PGM         F3IPGM
     C  N20              Z-ADD     DSDATE        F3IDT
      **** END   ADD **** 05/22/00 ************************************
      *
     C   60              WRITE     PFM01
     C  N60              UPDATE    PFM01
      *
     C                   ENDSR                                                  MVSCDB
