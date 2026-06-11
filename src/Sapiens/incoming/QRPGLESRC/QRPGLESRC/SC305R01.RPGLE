     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SC305R01 - REMARK TYPE CODE UPDATE/DISPLAY
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program updates and displays the DARTS Remark      **
     H**     Type Code file.                                         **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     SC305R - Work with Remark Type Codes                    **
     H**   CALLS TO:                                                 **
     H**     M0010R   - Work with Menus                              **
     H**     M0011R   - Work with Menu Options                       **
     H**     SC585R   - Remark Code Summary Table File               **
     H**     SRCVTDAT - Convert Date using CVTDAT Command            **
     H**     W0305F02 - WCMS DARTS Diary Information                 **
     H**                                                             **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED:   11/06/97      PROJECT: 87MCK                   **
     H*****************************************************************
     H* REVISIONS:                                                   *
     H*                                                              *
     H* 10/01/13 C:267 PROJECT: CLT99 Support: Redmine 3595          *
     H*          Initialize checkboxes to 'N' to avoid error         *
     H*          messages when the check boxes are left unchecked.   *
     H*                                                              *
     H* 04/08/04 C:128 PROJECT: IM099 Support: 42073                 *
     H*          Do not allow user to enter a single quote in the    *
     H*          description field.                                  *
     H*                                                              *
     H*                                                              *
     H* 12/02/02 C:128 PROJECT: VIMNT Visual Darts Maintenance       *
     H*          Added CTFL04 to screen. This flag disallows an add  *
     H*          or change to Additional Remarks after the initial   *
     H*          entry. Presently used only in Visual Darts.         *
     H*                                                              *
     H* 03/20/00 C:113 PROJECT: SUPPORT #21904                       *
     H*          Remove edit to require the allow folder/doc ID chg  *
     H*          to be 'Y' before # days for change to be entered.   *
     H*                                                              *
     H* 12/28/99 C:62 PROJECT:PR001                                  *
     H*               SWAP OUT WA001R WITH SA005R                    *
     H*                                                              *
     H* 11/19/99 C:33 PROJECT: DR078                                 *
     H*               ADD FIELD TO ALLOW ENTRY OF MESSAGE TEXT       *
     H*                                                              *
     H* 06/18/99 C: 53 PROJECT: BASE CHANGE                          *
     H*                UTILIZED API TO ACCESS MANAGED CARE INFO AND  *
     H*                CHECK FOR THE NON-PDS WORKERS COMP FLAG TO    *
     H*                CONTROL WCMS ACCESS TO W0305R02.              *
     H*                                                              *
     H****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      09   Reserved for maintenance/inquiry conditions
     H**      40   Add mode
     H**      41   Summ ref code used (DRFL02 = 'Y')
     H**      42   Purge allowed (DRFL04 = 'Y')
     H**      43   Data entry program used (DRFL03 = 'Y')
     H**      44   Auto folder build used (DRFL05 = 'Y')
     H**      60   Used for chaining
     H**      71   Reserved for screen error indicator
     H**      72   SCR01 SCM0503 Error - Required field (RMCD)
     H**      73   SCR01 SC40002 Error - Duplicate record (RMCD)
     H**      74   SCR01 SCM0503 Error - Required field (DESC)
     H**      75   SCR01 SCM0601 Error - Invalid summary code (SMCD)
     H**      76   SCR01 SCM0502 Error - Folder change not Y/N (FL03)
     H**      77   SCR01 SCM0503 Error - Required field (FL03)
     H**      78   SCR01 SCM0502 Error - Diary type not Y/N (FL01)
     H**      79   SCR01 SCM0503 Error - Required field (FL01)
     H**      80   SCR01 SC40003 Error - Diary type must be Y (DPRG)
     H**      81   SCR01 SC40004 Error - Diary entry must be used
     H**                                 (MNAM) (OPTN)
     H**      82   SCR01 SCM0615 Error - Invalid Menu/Option
     H**                                 (MNAM) (OPTN)
     H**      *** No longer used ***
     H**      83   SCR01 SC40005 Error - Allow chg must be Y (#DCH)
     H**
     H**      84 - 87 are reserved for screen error indicators
     H**      88   Position cursor after (MNAM) search
     H**      89   Position cursor after (SMCD) search
     H**      90   Used as general purpose error indicator
     H**      99   Used when chaining to file being updated
     H**
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      MMOPP  (POP01)  Menu/Option File                       **
     H**      MMOPL  (POP02)  Menu/Option File (MNAM/OPTN)           **
     H**      STCTP  (DBRCDF) Remark Type Table                      **
     H**      STCTL1 (PCT01)  Remark Type Table (RMCD)               **
     H**      STSML  (PSM01)  Remark Summary Code Table (SMCD)       **
     H**      SSYLF           Logical over system files              **
     H**             (SSY01)  Application System Control File (SY)   **
     H**             (SDR01)  DARTS Control File (SC)                **
     H*****************************************************************
     FSC305F01  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FSTCTP     UF A E             DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCT01:DBRCDF)
     FMMOPP     IF   E             DISK
     FMMOPL     IF   E           K DISK
     F                                     RENAME(POP01:POP02)
     FSTCTL1    IF   E           K DISK
     FSTSML     IF   E           K DISK
     FSSYLF     IF   E           K DISK
      *** 04/08/04 Begin Add ******************************************
      /COPY SCOPYMT01D
     D DA              S              1    DIM(40)
      *** 04/08/04 End   Add ******************************************
     D*================================================================
     D*  Data Structure and Table Definitions
     D*================================================================
     D***PY SCOPYMT01I
     D*
     D*  Relative record number for add
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Remark Type Codes')
     D COMAPI          C                   CONST('SA005R')
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
     C*** BEGIN DEL *** 12/28/99 **************************************
     C**********           PARM           ##FND   30
     C*** END   DEL *** 12/28/99 **************************************
     C                   PARM                    ##CONM           40
     C*** BEGIN DEL *** 12/28/99 **************************************
     C**********           PARM           ##FNAM 40
     C*** END   DEL *** 12/28/99 **************************************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*          POP01 - Menu/Option File
     C     OPKY          KLIST
     C                   KFLD                    SCMNAM
     C                   KFLD                    SCOPTN
     C*================================================================
     C*  Main
     C*================================================================
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
     C                   DO
     C*
     C*          Validate remark type
     C*             72 - SCM0503 Required field (RMCD)
     C*             73 - SC40002 Duplicate record (RMCD)
     C*          ------------------------------------------------------
     C     SCRMCD        IFEQ      *BLANKS
     C                   SETON                                        7290
     C                   LEAVE                                                  VVV Leave
     C                   ELSE                                                   RMCD = Blanks
     C     SCRMCD        CHAIN     PCT01                              60
     C  N60
     CAN 40              SETON                                        7390
     C  N60
     CAN 40              LEAVE
     C                   ENDIF                                                  RMCD = Blanks
     C*
     C*          Validate description
     C*             74 - SCM0503 Required field (DESC)
     C*          ------------------------------------------------------
     C     SCDESC        IFEQ      *BLANKS
     C                   SETON                                        7490
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  DESC = Blanks
      *** 04/08/04 Begin Add ******************************************
     C                   MOVEA     SCDESC        DA
     C                   MOVE      X'7D'         SINQTE            1
     C     SINQTE        LOOKUP    DA                                     98
     C     *IN98         IFEQ      *ON
     C                   SETON                                        8890
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF
      *** 04/08/04 End   Add ******************************************
     C*
     C*          Validate summary code
     C*             75 - SCM0601 Invalid summary code (SMCD)
     C*          ------------------------------------------------------
     C     SCSMCD        IFNE      *BLANK
     C     SCSMCD        CHAIN     PSM01                              60
     C   60              SETON                                        7590
     C   60              LEAVE                                                  VVV Leave
     C                   ENDIF                                                  SMCD = Blanks
     C*
     C*          Validate change of folder/doc ID
     C*             76 - SCM0502 Folder change not Y/N (FL03)
     C*             77 - SCM0503 Required field (FL03)
     C*          ------------------------------------------------------
     C     SCFL03        IFEQ      *BLANK
     C                   SETON                                        7790
     C                   LEAVE                                                  VVV Leave
     C                   ELSE                                                   FL03 = Blanks
     C     SCFL03        IFNE      'Y'
     C     SCFL03        ANDNE     'N'
     C                   SETON                                        7690
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  FL03 <> 'Y'
     C                   ENDIF                                                  FL03 = Blanks
     C*** BEGIN DEL *** 03/20/00 ***********************************************
     C**********
     C********** Validate # days for change of addtl remarks
     C**********    83 - SC40005 Allow change must be Y
     C********** ------------------------------------------------------
     C********** SC#DCH    IFNE 0
     C********** SCFL03    ANDNE'Y'
     C**********           SETON                     8390
     C**********           LEAVE                           VVV Leave
     C**********           ENDIF                           #DCH <> Blanks
     C*** END   DEL *** 03/20/00 ***********************************************
     C*
     C*          Validate diary type
     C*             78 - SCM0502 Diary type not Y/N (FL01)
     C*             79 - SCM0503 Required field (FL01)
     C*          ------------------------------------------------------
     C     SCFL01        IFEQ      *BLANK
     C                   SETON                                        7990
     C                   LEAVE                                                  VVV Leave
     C                   ELSE                                                   FL01 = Blanks
     C     SCFL01        IFNE      'Y'
     C     SCFL01        ANDNE     'N'
     C                   SETON                                        7890
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  FL01 <> 'Y'
     C                   ENDIF                                                  FL01 = Blanks
     C***  Begin Add  ***  12/02/02  *********************************
     C*
     C*          Validate Additional Remarks flag
     C*             70 - SCM0502 Flag not Y/N (FL04)
     C*          ------------------------------------------------------
     C     SCFL04        IFNE      'Y'
     C     SCFL04        ANDNE     'N'
     C                   SETON                                        7090
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  FL04 <> 'Y'
     C***  End   Add  ***  12/02/02  *********************************
     C*
     C*          Validate # days to keep on system/purge
     C*             80 - SC40003 Diary type must be Y (DPRG)
     C*          ------------------------------------------------------
     C     SCDPRG        IFNE      0
     C     SCFL01        ANDNE     'Y'
     C                   SETON                                        8090
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  DPRG <> Blanks
     C*
     C*          Validate memu/option
     C*             81 - SC40004 Diary entry must be used (MNAM) (OPTN)
     C*             82 - SCM0615 Invalid Menu/Option
     C*          ------------------------------------------------------
     C     SCDEPG        IFEQ      *BLANKS
     C     SCMNAM        ANDNE     *BLANKS
     C     SCDEPG        OREQ      *BLANKS
     C     SCOPTN        ANDNE     *BLANKS
     C                   SETON                                        8190
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  DEPG = Blanks
     C*
     C     SCMNAM        IFNE      *BLANKS
     C     SCOPTN        ORNE      *BLANKS
     C     OPKY          CHAIN     POP02                              60
     C   60              SETON                                        8290
     C   60              LEAVE                                                  VVV Leave
     C                   ENDIF                                                  MNAM <> Blanks
     C*
     C                   ENDDO                                                  DO Loop
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
     C  N40              MOVEL     CTSTAT        SCSTAT
     C                   MOVEL     CTTYPE        SCTYPE
     C                   MOVEL     CTDESC        SCDESC
     C                   MOVEL     CTFL01        SCFL01
     C                   MOVEL     CTFL03        SCFL03
     C***  Begin Add  ***  10/01/13  *********************************
     C     CTFL01        IFEQ      *BLANK
     C                   MOVE      'N'           SCFL01
     C                   ENDIF
     C     CTFL03        IFEQ      *BLANK
     C                   MOVE      'N'           SCFL03
     C                   ENDIF
     C***  End   Add  ***  10/01/13  *********************************
     C***  Begin Add  ***  12/02/02  *********************************
     C                   MOVEL     CTFL04        SCFL04
     C     CTFL04        IFEQ      *BLANK
     C                   MOVE      'N'           SCFL04
     C                   ENDIF
     C***  End   Add  ***  12/02/02  *********************************
     C                   MOVEL     CTSMCD        SCSMCD
     C     *IN40         IFEQ      *OFF
     C     SCSMCD        ANDNE     *BLANKS
     C     SCSMCD        CHAIN     PSM01                              60
     C                   MOVEL(P)  SMDESC        SCSMCT
     C                   ENDIF                                                  *IN40 = Off
     C                   MOVEL     CTDPRG        SCDPRG
     C                   MOVEL     CTDEPG        SCDEPG
     C                   MOVEL     CTRMCD        SCRMCD
     C                   MOVEL     CTDSLV        SCDSLV
     C                   MOVEL     CTMNAM        SCMNAM
     C                   MOVEL     CTOPTN        SCOPTN
     C                   MOVEL     CT#DCH        SC#DCH
     C                   MOVEL     CTDLSL        SCDLSL
     C                   MOVEL     CTCMSL        SCCMSL
     C                   MOVEL     CTCPSL        SCCPSL
     C***  BEGIN ADD  ***  11/19/99  *********************************
     C                   MOVEL(P)  CTDMSG        SCDMSG
     C***  END   ADD  ***  11/19/99  *********************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVEL     SCTYPE        CTTYPE
     C                   MOVEL     SCDESC        CTDESC
     C                   MOVEL     SCFL01        CTFL01
     C                   MOVEL     SCFL03        CTFL03
     C***  Begin Add  ***  12/02/02  *********************************
     C                   MOVEL     SCFL04        CTFL04
     C***  End   Add  ***  12/02/02  *********************************
     C                   MOVEL     SCSTAT        CTSTAT
     C                   MOVEL     SCSMCD        CTSMCD
     C                   MOVEL     SCDPRG        CTDPRG
     C                   MOVEL     SCDEPG        CTDEPG
     C                   MOVEL     SCRMCD        CTRMCD
     C                   MOVEL     SCDSLV        CTDSLV
     C                   MOVEL     SCMNAM        CTMNAM
     C                   MOVEL     SCOPTN        CTOPTN
     C                   MOVEL     SC#DCH        CT#DCH
     C                   MOVEL     SCDLSL        CTDLSL
     C                   MOVEL     SCCMSL        CTCMSL
     C                   MOVEL     SCCPSL        CTCPSL
     C***  BEGIN ADD  ***  11/19/99  *********************************
     C                   MOVEL(P)  SCDMSG        CTDMSG
     C***  END   ADD  ***  11/19/99  *********************************
     C*
     C                   MOVEL(P)  Q1USER        CTUSER
     C                   MOVEL(P)  Q1PGM         CTPGM
     C                   Z-ADD     DSTIME        CTTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTDT
     C     *IN40         IFEQ      *ON
     C                   MOVE      'A'           CTSTAT
     C                   MOVEL(P)  CTUSER        CTIUSR
     C                   MOVEL(P)  CTPGM         CTIPGM
     C                   Z-ADD     CTTIME        CTITIM
     C                   Z-ADD     CTDT          CTIDT
     C                   MOVEL     *BLANKS       SCSMCT
     C                   ENDIF                                                  *IN40 = On
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
     C*
     C*          Process Function 10: Search Provider
     C*          ------------------------------------------------------
     C     *INKJ         IFEQ      *ON
     C                   DO
     C     HFLD          IFEQ      'SCSMCD'
     C                   MOVE      *BLANKS       QQSMCD            4
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'SC585R'
     C                   PARM                    QQSMCD
     C                   PARM                    QQRTC
     C*
     C     QQRTC         IFEQ      'A'
     C                   MOVE      '1'           OUTFLG
     C                   LEAVE                                                  VVV LEAVE
     C                   ENDIF                                                  RTC = 'A'
     C*
     C                   SETON                                        90
     C     QQRTC         IFEQ      'B'
     C                   LEAVE                                                  VVV LEAVE
     C                   ENDIF                                                  RTC = 'B'
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVEL     QQSMCD        SCSMCD
     C     SCSMCD        CHAIN     PSM01                              60
     C                   MOVEL(P)  SMDESC        SCSMCT
     C                   SETON                                        89
     C                   LEAVE                                                  VVV LEAVE
     C                   ENDIF                                                  RTC = '*'
     C                   ENDIF                                                  HFLD = SRC
     C*
     C*          Process Function 10: Search Menu
     C*          ------------------------------------------------------
     C     HFLD          IFEQ      'SCMNAM'
     C                   MOVE      '2'           QQCALL            1
     C                   MOVE      *BLANKS       QQKC              1
     C                   MOVE      *BLANKS       QQKL              1
     C                   MOVE      *ZEROS        QQRRNR            9 0
     C                   CALL      'M0010R'
     C                   PARM                    QQCALL
     C                   PARM                    QQKC
     C                   PARM                    QQKL
     C                   PARM                    QQRRNR
     C*
     C     QQKC          IFEQ      '1'
     C                   MOVE      '1'           OUTFLG
     C                   LEAVE                                                  VVV LEAVE
     C                   ENDIF                                                  KC = '1'
     C*
     C                   SETON                                        90
     C     QQKL          IFEQ      '1'
     C                   LEAVE                                                  VVV LEAVE
     C                   ENDIF                                                  KL = '1'
     C*
     C     QQRRNR        IFNE      *ZEROS
     C     QQRRNR        CHAIN(N)  POP01                              99
     C                   MOVEL     OPMNAM        SCMNAM
     C                   SETON                                        88
     C                   LEAVE                                                  VVV LEAVE
     C                   ENDIF                                                  QQRRNR <> 0
     C                   ENDIF                                                  HFLD = MNAM
     C*
     C*          Process Function 10: Search option
     C*          ------------------------------------------------------
     C     HFLD          IFEQ      'SCOPTN'
     C                   MOVE      '2'           QQCALL            1
     C                   MOVE      *BLANKS       QQKC              1
     C                   MOVE      *BLANKS       QQKL              1
     C                   MOVE      *ZEROS        QQRRNR            9 0
     C                   CALL      'M0011R'
     C                   PARM                    QQCALL
     C                   PARM                    QQKC
     C                   PARM                    QQKL
     C                   PARM                    QQRRNR
     C*
     C     QQKC          IFEQ      '1'
     C                   MOVE      '1'           OUTFLG
     C                   LEAVE                                                  VVV LEAVE
     C                   ENDIF                                                  KC = '1'
     C*
     C                   SETON                                        90
     C     QQKL          IFEQ      '1'
     C                   LEAVE                                                  VVV LEAVE
     C                   ENDIF                                                  KL = '1'
     C*
     C     QQRRNR        IFNE      *ZEROS
     C     QQRRNR        CHAIN(N)  POP01                              99
     C                   MOVEL     OPOPTN        SCOPTN
     C                   SETON                                        88
     C                   LEAVE                                                  VVV LEAVE
     C                   ENDIF                                                  QQRRNR <> 0
     C                   ENDIF                                                  HFLD = OPTN
     C*
     C                   SETON                                        90
     C                   ENDDO                                                   DO Loop
     C                   ENDIF                                                   *INKJ = On
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*          Is W0305R02 needed: SYWCIF = Y, Diary Type <> blanks
     C*          ------------------------------------------------------
     C     @@MODE        IFEQ      'D'
     C     OUTFLG        ANDNE     '1'
     C     *IN45         ANDEQ     *ON
     C     CTFL01        ANDEQ     'Y'
     C                   EXSR      XWCMS
     C                   RETURN
     C                   ENDIF                                                  MODE = 'D'
     C*
     C*          Return function key settings and exit
     C*          ------------------------------------------------------
     C                   MOVE      *INKC         @@KC
     C                   MOVE      *INKL         @@KL
     C                   SETON                                        LR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C*          40 - Set on if 'Add' Mode
     C*          ------------------------------------------------------
     C     @@MODE        COMP      'A'                                    40
     C   40              MOVE      'A'           SCSTAT
     C*
     C*          Set on display selection indicators
     C*             41 if Summ ref code used (DRFL02 = 'Y')
     C*             42 if Purge allowed (DRFL04 = 'Y')
     C*             43 if Data entry program used (DRFL03 = 'Y')
     C*             44 if Auto folder build used (DRFL05 = 'Y')
     C*             45 if WCMS on system (SYWCIF = 'Y')
     C*** 11/19/99 *** ADD CHECK FOR VDARTS INSTALLED?
     C*             46 if VDART on system (DRFL10 = 'Y')
     C*** 11/19/99 *** END ADD
     C*          ------------------------------------------------------
     C     'SC'          CHAIN     SDR01                              60
     C     *IN60         IFEQ      *OFF
     C     DRFL02        COMP      'Y'                                    41
     C     DRFL04        COMP      'Y'                                    42
     C     DRFL03        COMP      'Y'                                    43
     C     DRFL05        COMP      'Y'                                    44
     C*** 11/19/99 *** ADD CHECK FOR VDARTS INSTALLED?
     C     DRFL10        COMP      'Y'                                    46
     C*** 11/19/99 *** END ADD
     C                   ENDIF                                                  *IN60 = Off
     C*
     C*** BEG ADD *** 06/18/99 *****************************
     C*
     C* CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
     C*
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
     C                   PARM                    ##AR              1            MC AR INST
     C                   PARM                    ##NTWK            1            NETWORK INST
     C                   PARM                    ##UR              1            PRECERT INST
     C                   PARM                    ##MED             1            MEDMGMT INST
     C                   PARM                    ##EDI             1            EDI INST
     C                   PARM                    ##WCDK            1            WC DARTS KEY
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILLS
     C                   PARM                    ##NPWC            1            NON-PDS WC  LS
     C*
     C     ##WCIF        IFEQ      'Y'
     C     ##NPWC        ANDNE     'Y'
     C                   MOVE      *ON           *IN45
     C                   ENDIF
     C*
     C*** END ADD *** 06/18/99 *****************************
     C*** BEG DEL *** 06/18/99 *****************************
     C*********  'SY'      CHAINSSY01                60
     C*********  *IN60     IFEQ *OFF
     C*********  SYWCIF    COMP 'Y'                      45
     C*********            ENDIF                           *IN60 = Off
     C*** END DEL *** 06/18/99 *****************************
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C*          MODE 'S' - Change status
     C*          ------------------------------------------------------
     C     @@MODE        IFEQ      'S'
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C     CTSTAT        IFEQ      'I'
     C                   MOVE      'A'           CTSTAT
     C                   ELSE                                                   STAT = 'I'
     C                   MOVE      'I'           CTSTAT
     C                   ENDIF                                                  STAT = 'I'
     C                   MOVEL     Q1USER        CTUSER
     C                   MOVEL     Q1PGM         CTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTDT
     C                   MOVEL     DSTIME        CTTIME
     C                   UPDATE    DBRCDF
     C                   ENDIF                                                  *IN99 = Off
     C*
     C                   RETURN
     C                   ENDIF                                                  MODE = 'S'
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
     C*
     C*          Is W0305R02 needed: SYWCIF = Y, Diary Type <> blanks
     C*          ------------------------------------------------------
     C     *IN45         IFEQ      *ON
     C     CTFL01        ANDEQ     'Y'
     C   40              MOVE      'C'           @@MODE
     C                   EXSR      XWCMS
     C   40              MOVE      'A'           @@MODE
     C   KL              SETON                                        90
     C  NKL              RETURN
     C                   ENDIF                                                  *IN45 = On
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      STCTP
     C*
     C*          Is W0305R02 needed: SYWCIF = Y, Diary Type <> blanks
     C*          ------------------------------------------------------
     C     *IN45         IFEQ      *ON
     C     CTFL01        ANDEQ     'Y'
     C   40              MOVE      'C'           @@MODE
     C                   EXSR      XWCMS
     C   40              MOVE      'A'           @@MODE
     C   KL              RETURN
     C                   ENDIF                                                  *IN45 = On
     C                   ENDSR
     C*================================================================
     C* XWCMS - Transfer to WCMS program
     C*================================================================
     C     XWCMS         BEGSR
     C*
     C*          Is W0305R02 needed: SYWCIF = Y, Diary Type <> blanks
     C*          ------------------------------------------------------
     C     *IN45         IFEQ      *ON
     C     CTFL01        ANDEQ     'Y'
     C                   Z-ADD     DBRRN         @@RRN
     C                   CALL      'W0305R02'
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C     @@KC          IFEQ      *ON
     C     @@KL          OREQ      *ON
     C                   MOVE      @@KC          *INKC
     C                   MOVE      @@KL          *INKL
     C                   ENDIF                                                  KC = On
     C                   ENDIF                                                  *IN45 = On
     C                   ENDSR
