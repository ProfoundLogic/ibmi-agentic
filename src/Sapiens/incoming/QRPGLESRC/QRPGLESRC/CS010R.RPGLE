     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE CS010R - CLAIM SCAN PROMPT PROGRAM
     H*****************************************************************
     H* REVISIONS:
     H*
     H*  02/10/98  C:62  PROJECT#: MC145
     H*                  ** USED CONVERSION FROM MDYY YO CYMD
     H*
     H*  11/11/97  C:86   PROJECT#: CS010
     H*                FOR SCREEN 3
     H*         1.  EXPAND ACCIDENT DATE FIELD TO 8.0N
     H*         2.  REMOVE INJTYP AND DAYLST HEADINGS AND FIELDS
     H*         3.  REMOVE USE OF HIDDEN FIELDS SUB BATCH & DELETE CODE
     H*         4.  ADD BODY PART HEADING AND FIELD AND HIDDEN FIELD
     H*             DEATH DATE
     H*                FOR SCREEN 4
     H*         1.  EXPAND ACCIDENT DATE FIELD TO 8.0N
     H*         2.  REMOVE INJTYP AND DAYLST HEADINGS AND FIELDS
     H*         3.  ADD BODY PART HEADING AND FIELD
     H*   08/16/94   C:26  PROJECT#: CS004
     H*                    1. ADD F6 CLIENT ALPHA SEARCH
     H*                    2. ADD STANDARD SCREEN HEADER FIELDS
     H*                    3. ADD COMPANY AND USER FILES
     H* 12/14/90 LJD   PER TLB:
     H*                1.  BREAK OUT EMPLOYEE NAME INTO LAST,FIRST,MI
     H*                    THIS CHANGES SCR02,SCR03,SCR04 PROCESSING
     H*                    WRT PROMPTS TO REPOSITION FILE
     H*                    THIS CHANGES SCRNME TO SCRLST,SCRFRT,SCRMI
     H*                    THIS CHANGES SCUNME TO SCULST,SCUFRT,SCUMI
     H*                    THIS CHANGES LENGTH OF SCEMNM FROM 18 TO 16
     H*                2.  CHANGED CLIENT FILE TO CMCTP/CMCTL1
     H*                    SCR01 - F5=CLIENT SRCH CHGD TO CS540R
     H*                    SCR01 - F7=ADD A CLIENT CHGD TO CS040R
     H*                3.  IF BILLING IS INSTALLED IN CLAIM SCAN SYSTEM:
     H*                    ADDED F23 TO BYPASS 2ND BILLING NOTICE
     H*
     H*   CHGD RETRIEVAL ON DATA AREA XREF# BECAUSE IT WASN'T ALLOWING
     H*   MULTIPLE USERS INTO THE PROGRAM AT THE SAME TIME.  REPLACED
     H*   DATA AREA XREF# WITH A PHYSICAL FILE XREF#.
     H*
     H*****************************************************************
      * INDICATOR USAGE:
      *  60    READC ON SUBFILES
      *  61    SETLL/READ ON CLAIM SCAN FILE
      *  62    GENERAL CHAIN ON SUBFILES AND WORK FILES
      * SCR01:
      *  20    CSM0503-CLIENT NUMBER REQUIRED
      *  21    CSM0505-INVALID CLIENT NUMBER
      *  22    CSM0506-M/T/F SEARCH FLAG MUST BE 'M' OR 'T' OR 'F'
      *  23    CLIENT HAS RECEIVED A 2ND BILLING NOTICE, ACTIVATE F23,
      *        DISPLAY INFORMATIONAL/ERROR MESSAGE
      *  24    POSITION CURSOR AT SEARCH TYPE AFTER SEARCH ON CLIENT
      * SCR02:
      *  20    CSM0503-SS# REQUIRED
      *  21    CSM0503-EMPLOYEE LAST NAME REQUIRED
      * SCR03:
      *  20    CSM0507-REQUESTED ITEM MUST BE MARKED WITH '1' OR BLANK
      *  21    PROTECT ITEM OPTION ONCE MARKED FOR SELECTION
      *  22    HIGHLIGHT ITEM IF MATCHES USER PROMPT SELECTION
      *  23    PROTECT ITEM OPTION IF ITS BEEN SELECTED FOR SAME CLIENT IN
      *        SAME JOB/FILE MEMBER FROM A PREVIOUS REQUEST
      *  91    SEARCH BY SS#
      *  92    SEARCH BY EMPLOYEE LAST NAME
      *  93    SEARCH BY EMPLOYER NUMBER
      * SCR04:
      *  25    CSM0507-REQUESTED ITEM MUST BE MARKED WITH '4' OR BLANK
      *****************************************************************
      * FIELD NAMES:
      * SCACDT = SUBFILE ACCIDENT DATE SEARCH ITEM
      * SCAREA = HIDDEN SUBFILE AREA ITEM
      * SCBTDT = HIDDEN SUBFILE BATCH DATE ITEM
      * SCCARR = HIDDEN SUBFILE CARRIER NUMBER ITEM
      * SCCLT# = CLIENT NUMBER
      * SCDTYP = SUBFILE DISABILITY TYPE SEARCH ITEM
      * SCDIV  = HIDDEN SUBFILE EMPLOYER SUFFIX ITEM
      * SCEMNC = HIDDEN SUBFILE COMPLETE EMPLOYER NAME ITEM
      * SCEMNM = SUBFILE EMPLOYER NAME SEARCH ITEM
      * SCEMP# = SUBFILE EMPLOYER NUMBER ITEM
      * SCITMS = # OF ITEMS SELECTED FOR SEARCH (SCR04)
      ****** BEGIN ADD ************ 11/11/97 ************************
      * SCPART = PART OF BODY TYPE SEARCH ITEM
      * SCDEDT = HIDDEN SUBFILE DEATH DATE ITEM
      ****** END   ADD ************ 11/11/97 ************************
      ****** BEGIN DELETE ********* 11/11/97 ************************
      * SCDLST = SUBFILE DAYS LOST SEARCH ITEM    ** NO LONGER USED
      * SCITYP = SUBFILE INJURY TYPE SEARCH ITEM  ** NO LONGER USED
      * SCDLCD = HIDDEN SUBFILE DELETE CODE ITEM  ** NO LONGER USED
      * SCSBBT = HIDDEN SUBFILE SUB BATCH ITEM    ** NO LONGER USED
      ****** END   DELETE ********* 11/11/97 ************************
      * SCMTF  = M OR T OR F SEARCH FLAG
      * SCLAST = SUBFILE EMPLOYEE LAST NAME SEARCH ITEM
      * SCLPDT = SCR01 - LAST PAID DATE DISPLAYED IF 2ND BILLING NOTICE = 'Y'
      * SCFRST = SUBFILE EMPLOYEE FIRST NAME SEARCH ITEM
      * SCMI   = SUBFILE EMPLOYEE MIDDLE INITIAL NAME SEARCH ITEM
      * SCMSG1 = SCR01 2ND BILLING NOTICE INFORMATIONAL MESSAGE
      * SCRFRT = USER REQUESTED EMPLOYEE FIRST NAME FROM SCR02
      * SCRLST = USER REQUESTED EMPLOYEE LAST NAME FROM SCR02
      * SCRMI  = USER REQUESTED EMPLOYEE MIDDLE INITIAL FROM SCR02
      * SCRSS# = USER REQUESTED SS# FROM SCR02
      * SCSIC  = HIDDEN SUBFILE SIC IND # ITEM
      * SCSS#  = SUBFILE SS# SEARCH ITEM
      * SCUEM# = USER EMPLOYER NUMBER SEARCH SELECTION
      * SCUFRT = USER EMPLOYEE FIRST NAME SEARCH SELECTION
      * SCULST = USER EMPLOYEE LAST NAME SEARCH SELECTION
      * SCUMI  = USER EMPLOYEE MIDDLE INITIAL SEARCH SELECTION
      * SCUSS# = USER SS# SEARCH SELECTION
      *****************************************************************
      * FILE SPECIFICATIONS  * * *
      *****************************************************************
     FCS010F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR03S:RRN3)
     F                                     SFILE(SCR04S:RRN4)
     F*
     F*** CLIENT MASTER FILE
     FCMCTL1    IF   E           K DISK
     F*
     F*** CLAIM SCAN MASTER FILE - SS# ORDER
     FCMCSL1    IF   E           K DISK
     F*
     F*** CLAIM SCAN MASTER FILE - EMPLOYEE NAME ORDER
     FCMCSL2    IF   E           K DISK
     F                                     RENAME(PCS01:PCS02)
     F*
     F*** CLAIM SCAN MASTER FILE - EMPLOYER NUMBER ORDER
     FCMCSL3    IF   E           K DISK
     F                                     RENAME(PCS01:PCS03)
     F*
     F*** CLAIM SCAN WORK FILE - SS#/ACDT ORDER
     FCWCSP     UF A E           K DISK
     F                                     RENAME(PCS01:PCS04)
     F*
     F*** CLAIM SCAN CONTROL FILE
     FSSYLF     IF   E           K DISK
     F*
     F*** CLIENT SUMMARY WORK FILE
     FCWCLP     O    E           K DISK
     F*
     F*** CLIENT SUMMARY (CWCLP)/EMPLOYEE DETAIL (CWCSP) XREF NUMBER COUNTER
     FXREF#     UF   E           K DISK
     F*
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK

     D MSG1            S             60    DIM(1) CTDATA PERRCD(1)              SCR01 MESSAGE
     D MSG4            S             10    DIM(1) CTDATA PERRCD(1)              SCR04 MESSAGE
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D* * *  FILE INFORMATION DATA STRUCTURE
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D* * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C****************************************************************
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE CLAIM SCAN MASTER FILE - SS# ORDER
     C*
     C     CSKEY1        KLIST
     C                   KFLD                    SCUSS#
     C*
     C*   KEY LIST FOR THE CLAIM SCAN MASTER FILE - EMPLOYEE NAME ORDER
     C*
     C     CSKEY2        KLIST
     C                   KFLD                    SCULST
     C                   KFLD                    SCUFRT
     C                   KFLD                    SCUMI
     C*
     C*   KEY LIST FOR THE CLAIM SCAN MASTER FILE - EMPLOYER NUMBER ORDER
     C*
     C     CSKEY3        KLIST
     C                   KFLD                    SCUEM#
     C*
     C*   KEY LIST FOR THE CLAIM SCAN WORK FILE - SS#/ACDT ORDER
     C*
     C     CSKEY4        KLIST
     C                   KFLD                    XXSS#
     C                   KFLD                    XXACDT
     C*
     C*   ENTRY LIST
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
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
     C                   MOVE      *BLANKS       QQRTCD
     C*
      *------------------------*
      *** DEFINE VARIABLES     *
      *------------------------*
     C     *LIKE         DEFINE    CSSS#         XXSS#
     C     *LIKE         DEFINE    CSACDT        XXACDT
     C     *LIKE         DEFINE    CSKEY         CSKEY5
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   ELSE
     C                   Z-ADD     UPCO#         QQCO#             3 0
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C*
     C     TOP           TAG
     C*
     C*   INITIALIZE ROUTINE * * *
     C*
     C                   EXSR      INIT
     C*
     C*   DISPLAY ALL FORMATS
     C*
      *------------------------*
      *** DISPLAY POINT
      *------------------------*
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C     Q2RECF        OREQ      'SCR03C'
     C     Q2RECF        OREQ      'SCR03S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR03
     C                   EXFMT     SCR03C
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C     Q2RECF        OREQ      'SCR04C'
     C     Q2RECF        OREQ      'SCR04S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR04
     C                   EXFMT     SCR04C
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C/EJECT
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C     *IN11         IFEQ      '1'
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C* NOTE:  WILL RETURN TO CS010C AND REMOVE MEMBER - NO SEARCH DONE
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*   F5                                 *
     C****************************************
     C     *INKE         IFEQ      '1'
     C****************************
     C*   SCR01 = CLIENT SEARCH  *
     C****************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      SCCLT#        QQCLT#            5
     C                   CALL      'CS540R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCLT#
     C                   PARM                    QQRTCD            1
     C                   MOVE      QQCLT#        SCCLT#
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCCLT#        OREQ      *ZEROS
     C     QQRTCD        OREQ      '*'
     C                   MOVE      '1'           *IN24                          POS CURS SRCH TYPE
     C                   GOTO      STRT
     C                   END                                                    ENDEQ 'B'
     C*
     C                   END                                                    END SCR01
     C****************************
     C*   SCR03 = REFRESH        *
     C****************************
     C     Q2RECF        IFEQ      'SCR04'
     C     Q2RECF        OREQ      'SCR04S'
     C     Q2RECF        OREQ      'SCR04C'
     C*
     C* * * CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR04C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN4
     C                   MOVE      *BLANKS       XXSS#
     C                   Z-ADD     *ZERO         XXACDT
     C*
     C     CSKEY4        SETLL     PCS04                              61
     C                   EXSR      LOAD4
     C*
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END SCR04
     C*
     C                   GOTO      STRT
     C                   END                                                    END *INKE
     C****************************************
     C*   F6                                 *
     C****************************************
     C     *INKF         IFEQ      '1'
     C**********************************
     C*   SCR01 = CLIENT ALPHA SEARCH  *
     C**********************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      SCCLT#        QQCLT#
     C                   MOVE      *BLANKS       QQNAME
     C                   CALL      'CS541R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCLT#
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQNAME           40
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      QQNAME        SCNAME
     C                   MOVE      QQCLT#        SCCLT#
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCCLT#        OREQ      *ZEROS
     C     QQRTCD        OREQ      '*'
     C                   MOVE      '1'           *IN24                          POS CURS SRCH TYPE
     C                   GOTO      STRT
     C                   END                                                    ENDEQ 'B'
     C*
     C                   GOTO      STRT
     C                   END                                                    END SCR01
     C                   END                                                    END SCR01
     C****************************************
     C*   F7 = ADD A CLIENT                  *
     C****************************************
     C     *INKG         IFEQ      '1'
     C                   MOVE      '7'           QQRTCD
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'CS040R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQRTCD            1
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END                                                    END SCR01
     C                   GOTO      STRT
     C                   END                                                    END *INKG
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C     Q2RECF        OREQ      'SCR03S'
     C     Q2RECF        OREQ      'SCR03C'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C     Q2RECF        OREQ      'SCR04S'
     C     Q2RECF        OREQ      'SCR04C'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END *INKL
     C****************************************
     C*   F15 = SEARCH COMPLETE              *
     C****************************************
     C* NOTE:  WILL RETURN TO CS010C, SUBMIT PRINT REQUEST, AND
     C*        ESTABLISH A NEW MEMBER (FOR NEW CLIENT)
     C     *INKP         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *INKP
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C*
     C*** CHECK FOR END OF FILE (EITHER SCREEN)
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C*
     C************
     C*   SCR03  *
     C************
     C     Q2RECF        IFEQ      'SCR03'
     C     Q2RECF        OREQ      'SCR03S'
     C     Q2RECF        OREQ      'SCR03C'
     C                   EXSR      LOAD3
     C                   GOTO      STRT
     C                   END                                                    END EQ SCR03
     C************
     C*   SCR04  *
     C************
     C     Q2RECF        IFEQ      'SCR04'
     C     Q2RECF        OREQ      'SCR04S'
     C     Q2RECF        OREQ      'SCR04C'
     C                   EXSR      LOAD4
     C                   GOTO      STRT
     C                   END                                                    END EQ SCR04
     C***
     C                   END                                                    END 1 *IN18 ROLLUP
     C****************************************
     C*   F23 = BYPASS 2ND BILLING NOTICE    *
     C****************************************
     C* NOTE:  THIS WILL ALLOW THE USER TO BYPASS THE 2ND BILLING NOTICE
     C*        TO PERFORM THE SEARCH
     C     *INKX         IFEQ      '1'
     C                   GOTO      BYPASS
     C                   END                                                    END *INKP
     C*******
     C                   END                                                    END CMDKEYS
     C*****************************************************************
     C*   GOTO THE APPROPRIATE SCREEN
     C*****************************************************************
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C     Q2RECF        CABEQ     'SCR03S'      SCR03T
     C     Q2RECF        CABEQ     'SCR03C'      SCR03T
     C*
     C     Q2RECF        CABEQ     'SCR04'       SCR04T
     C     Q2RECF        CABEQ     'SCR04S'      SCR04T
     C     Q2RECF        CABEQ     'SCR04C'      SCR04T
     C*
     C*   PROCESS REQUESTED SCREEN
     C*
      *------------------------*
     C     SCR01T        TAG
      *------------------------*
     C*
     C*** CLIENT NUMBER REQUIRED
     C     SCCLT#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*** CLIENT NUMBER MUST BE ON FILE (VALID)
     C     SCCLT#        CHAIN     PCT01                              21
     C     *IN21         CABEQ     '1'           STRT
     C                   MOVE      *BLANKS       SCCLNM
     C                   MOVEL     CTNAME        SCCLNM
     C*
     C*** IF THIS SYSTEM HAS BILLING INSTALLED, THEN DETERMINE:
     C*** IF CLIENT'S 2ND BILLING NOTICE FLAG IS 'Y', DON'T ALLOW SEARCH
     C***    UNLESS THEY PRESS F23 TO OVERRIDE
     C                   MOVE      *BLANKS       SCMSG1
     C                   Z-ADD     *ZEROS        SCLPDT
     C                   MOVE      '0'           *IN23
     C*
     C                   MOVEL     'CS'          CSKEY5
     C     CSKEY5        CHAIN     SCS01                              60
     C     *IN60         IFEQ      '0'
     C     CSBLIN        ANDEQ     'Y'
     C*
     C     CT2NOT        IFEQ      'Y'
     C                   MOVEL     MSG1(1)       SCMSG1
     C*
     C                   Z-ADD     CTLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCLPDT
     C*
     C                   MOVE      '1'           *IN23
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END 2NOT = Y
     C                   END                                                    END BLIN = Y
     C*
     C     BYPASS        TAG
     C*
     C*** M/T/F SEARCH FLAG MUST BE M OR T OR F
     C     SCMTF         IFNE      'M'
     C     SCMTF         ANDNE     'T'
     C     SCMTF         ANDNE     'F'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*** LOAD SCREEN 2
     C                   MOVE      *BLANKS       SCRSS#
     C                   MOVE      *BLANKS       SCRLST
     C                   MOVE      *BLANKS       SCRFRT
     C                   MOVE      *BLANKS       SCRMI
     C*
     C*** GOTO SCREEN 2
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C*------------------------*
     C     SCR02T        TAG
     C*------------------------*
     C*
     C*** SS# IS REQUIRED
     C     SCRSS#        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*** EMPLOYEE NAME IS REQUIRED
     C     SCRLST        IFEQ      *BLANKS                                      12/14/90 CHG
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*** LOAD SCREEN 3
     C*** DEFAULT TO SOCIAL SECURITY SEARCH
     C                   MOVE      '1'           *IN91
     C                   MOVE      '0'           *IN92
     C                   MOVE      '0'           *IN93
     C                   MOVEL     SCRSS#        SCUSS#
     C                   MOVEL     SCRLST        SCULST
     C                   MOVEL     SCRFRT        SCUFRT
     C                   MOVEL     SCRMI         SCUMI
     C                   Z-ADD     *ZEROS        SCUEM#
     C*
     C*** GOTO SCREEN 3
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   EXSR      INIT3
     C                   EXSR      LOAD3
     C*
     C                   GOTO      STRT
     C*
      *------------------------*
     C     SCR03T        TAG
      *------------------------*
     C*
     C*** CHECK FOR USER SELECTION TO REPOSITION
     C     SCUSS#        IFNE      *BLANKS
     C                   MOVE      '1'           *IN91
     C                   MOVE      '0'           *IN92
     C                   MOVE      '0'           *IN93
     C                   EXSR      INIT3
     C                   EXSR      LOAD3
     C                   GOTO      STRT
     C                   END
     C*
     C*** CHECK FOR USER SELECTION TO REPOSITION
     C*
     C     SCULST        IFNE      *BLANKS
     C                   MOVE      '0'           *IN91
     C                   MOVE      '1'           *IN92
     C                   MOVE      '0'           *IN93
     C                   EXSR      INIT3
     C                   EXSR      LOAD3
     C                   GOTO      STRT
     C                   END
     C*
     C*** CHECK FOR USER SELECTION TO REPOSITION
     C     SCUEM#        IFNE      *ZEROS
     C                   MOVE      '0'           *IN91
     C                   MOVE      '0'           *IN92
     C                   MOVE      '1'           *IN93
     C                   EXSR      INIT3
     C                   EXSR      LOAD3
     C                   GOTO      STRT
     C                   END
     C*
     C*** OPTION SELECTION MUST BE EQUAL TO '1'
     C                   READC     SCR03S                                 60
     C     *IN60         DOWEQ     '0'
     C     SCSEL3        IFNE      *BLANK
     C*** IF THE SUBFILE HAS BEEN CHANGED, VERIFY THAT THE USER USED A '1'
     C*** TO INDICATE A SELECTED RECORD.
     C     SCSEL3        IFNE      '1'
     C                   MOVE      '1'           *IN20
     C                   MOVE      '0'           *IN21
     C                   UPDATE    SCR03S
     C                   GOTO      STRT
     C                   ELSE                                                   ELS NE '1'
     C*** IF THE SUBFILE HAS BEEN CHANGED AND THE USER HAS SELECTED A RECORD,
     C*** WRITE RECORDS TO CLAIM SCAN WORK FILE
     C                   EXSR      WRDET
     C*** THEN PROTECT SELECTION
     C                   MOVE      '1'           *IN21
     C                   UPDATE    SCR03S
     C                   END                                                    END NE '1'
     C                   END                                                    END NE *BLANK
     C*
     C                   READC     SCR03S                                 60
     C                   END                                                    END DOWEQ *IN60
     C*
     C*** GOTO SCREEN 4
     C*
     C                   MOVE      *BLANKS       XXSS#
     C                   Z-ADD     *ZEROS        XXACDT
     C                   EXSR      INIT4
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C*
     C                   GOTO      STRT
     C*
      *------------------------*
     C     SCR04T        TAG
      *------------------------*
     C*
     C                   MOVE      *BLANKS       SCNCHG            1            SCREEN CHG FLAG
     C*
     C*** OPTION SELECTION MUST BE EQUAL TO '4'
     C                   READC     SCR04S                                 60
     C     *IN60         DOWEQ     '0'
     C*
     C     SCSEL4        IFNE      *BLANKS
     C*** IF THE SUBFILE HAS BEEN CHANGED, VERIFY THAT THE USER USED A '4'
     C*** TO INDICATE A SELECTED RECORD.
     C     SCSEL4        IFNE      '4'
     C                   MOVE      '1'           *IN25
     C                   UPDATE    SCR04S
     C                   GOTO      STRT
     C*
     C                   ELSE                                                   ELS NE '4'
     C*
     C*** IF THE SUBFILE HAS BEEN CHANGED AND THE USER HAS SELECTED A RECORD,
     C*** THEN RETAIN SS#/ACDT FOR CHAIN TO OTHER FILES...
     C                   MOVEL     SCSS#         XXSS#
     C*** BEGIN CHG *** 02/10/98 **************************************
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
     C*** END   CHG *** 02/10/98 **************************************
     C*
     C*** THEN UPDATE SCR04...
     C                   MOVE      *BLANKS       SCSEL4
     C                   MOVEL     MSG4(1)       SCSS#
     C                   MOVE      *BLANKS       SCLAST
     C                   MOVE      *BLANKS       SCFRST
     C                   MOVE      *BLANKS       SCMI
     C*
     C                   MOVE      *BLANKS       SCACDT
     C                   MOVE      *BLANKS       SCEMP#
     C                   MOVE      *BLANKS       SCEMNM
     C***** BEGIN ADD  ****  11/11/97  ******************** PART 4 ****
     C                   MOVE      *BLANKS       SCPART
     C***** END   ADD  ****  11/11/97  ******************** PART 4 ****
     C***** BEGIN DELETE **  11/11/97  ******************** PART 2 ****
     C*******              MOVE *BLANKS   SCITYP
     C*******              MOVE *BLANKS   SCDLST
     C***** END   DELETE **  11/11/97  ******************** PART 2 ****
     C                   MOVE      *BLANKS       SCDTYP
     C*** HIDDEN SUBFILE FIELDS
     C                   MOVE      *BLANKS       SCDIV
     C                   MOVE      *BLANKS       SCCARR
     C                   MOVE      *BLANKS       SCBTDT
     C                   MOVE      *BLANKS       SCSIC
     C                   MOVE      *BLANKS       SCAREA
     C***** BEGIN ADD  ****  11/11/97  ********************* PART 4 ***
     C                   MOVE      *BLANKS       SCDEDT
     C***** END   ADD  ****  11/11/97  ********************* PART 4 ***
     C***** BEGIN DELETE **  11/11/97  ********************* PART 3 ***
     C*******              MOVE *BLANKS   SCSBBT
     C*******              MOVE *BLANKS   SCDLCD
     C***** END   DELETE **  11/11/97  ********************* PART 3 ***
     C                   MOVE      *BLANKS       SCEMNC
     C*
     C                   SUB       1             SCITMS
     C                   UPDATE    SCR04S
     C*
     C*** THEN DELETE RECORD FROM WORKFILE
     C     CSKEY4        CHAIN     PCS04                              70
     C     *IN70         IFEQ      '0'
     C*
     C*** UPDATE SCR03 SUBFILE SO THAT DELETED RECORD IS NO LONGER MARKED AS
     C*** A SELECTED RECORD
     C     CSRRN3        CHAIN     SCR03S                             62
     C     *IN62         IFEQ      '0'
     C                   MOVE      '0'           *IN21
     C                   MOVE      *BLANKS       SCSEL3
     C                   UPDATE    SCR03S
     C                   END                                                    END 0 *IN62
     C*
     C                   DELETE    PCS04
     C                   END                                                    END 0 *IN70
     C*
     C                   END                                                    END NE '4'
     C*
     C*** 'SCNCHG' FLAG INDICATES THAT A CHANGE TO THE SCREEN HAS OCCURRED
     C                   MOVE      'Y'           SCNCHG
     C                   END                                                    END NE *BLANKS
     C*
     C                   READC     SCR04S                                 60
     C                   END                                                    END DOWEQ *IN60
     C*
     C*** IF A CHANGE TO THE SCREEN HAS OCCURRED, REDISPLAY THE SCREEN
     C     SCNCHG        IFEQ      'Y'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END 'Y' CHG
     C*
     C*
     C*** ELSE IF NO CHANGE TO THE SCREEN HAS OCCURRED,
     C*** ALL EDITS PASSED, WRITE RECORDS TO CLAIM SCAN WORK FILE
     C*
     C                   EXSR      WRSUM
     C*
     C*** LOAD SCREEN 2
     C                   MOVE      *BLANKS       SCRSS#
     C                   MOVE      *BLANKS       SCRLST
     C                   MOVE      *BLANKS       SCRFRT
     C                   MOVE      *BLANKS       SCRMI
     C                   Z-ADD     *ZEROS        SCITMS
     C*
     C*** GOTO SCREEN 2
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     TERM          TAG
     C*
     C                   RETURN
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C     INIT          BEGSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     0             RRN3              5 0
     C                   Z-ADD     0             RRN4              5 0
     C*
     C                   Z-ADD     *ZEROS        SCCLT#
     C                   MOVE      *BLANKS       SCMTF
     C                   Z-ADD     *ZEROS        SCITMS
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   ENDSR
      *====================================================================*
      * * *   INIT3 SUBROUTINE                                 I N I T 2 ***
      *====================================================================*
     C     INIT3         BEGSR
     C                   Z-ADD     0             RRN3
     C                   MOVE      '0'           *IN21
     C                   MOVE      '0'           *IN23
     C                   MOVE      *BLANK        SCSEL3
     C*
     C   91CSKEY1        SETLL     PCS01
     C   92CSKEY2        SETLL     PCS02
     C   93CSKEY3        SETLL     PCS03
     C*
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR03C
     C                   MOVE      '0'           *IN04
     C*
     C                   ENDSR
      *====================================================================*
      * * *   INIT4 SUBROUTINE                                 I N I T 3 ***
      *====================================================================*
     C     INIT4         BEGSR
     C                   Z-ADD     0             RRN4
     C*
     C*** REPOSITION FILE
     C     CSKEY4        SETLL     PCS04
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR04C
     C                   MOVE      '0'           *IN04
     C*
     C*** LOAD SCREEN
     C                   EXSR      LOAD4
     C*
     C                   ENDSR
     C*====================================================================*
     C* * *  LOAD3 SUBROUTINE                                    L O A D ***
     C*====================================================================*
     C     LOAD3         BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        13            X                 5 0
     C*
     C   91              READ      PCS01                                  61
     C   92              READ      PCS02                                  61
     C   93              READ      PCS03                                  61
     C*
     C     *IN61         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN61         IFEQ      '0'
     C                   ADD       1             RRN3
     C                   MOVEL     CSSS#         SCSS#
     C                   MOVEL     CSLAST        SCLAST
     C                   MOVEL     CSFRST        SCFRST
     C                   MOVEL     CSMI          SCMI
     C***** BEGIN CHANGE **  11/11/97  ********************* PART 1 ***
     C********             Z-ADDCSACDT    SCACDT
     C                   Z-ADD     CSACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C***** END CHANGE ****  11/11/97  ********************* PART 1 ***
     C                   Z-ADD     CSEMP#        SCEMP#
     C                   MOVEL     CSEMNM        SCEMNM
     C***** BEGIN ADD  ****  11/11/97  ********************* PART 4 ***
     C                   MOVEL     CSPART        SCPART
     C***** END   ADD  ****  11/11/97  ********************* PART 4 ***
     C***** BEGIN DELETE **  11/11/97  ********************* PART 2 ***
     C*******              Z-ADDCSITYP    SCITYP
     C*******              Z-ADDCSDLST    SCDLST
     C***** END   DELETE **  11/11/97  ********************* PART 2 ***
     C                   Z-ADD     CSDTYP        SCDTYP
     C*** HIDDEN SUBFILE FIELDS
     C                   Z-ADD     CSDIV         SCDIV
     C                   Z-ADD     CSCARR        SCCARR
     C                   Z-ADD     CSBTDT        SCBTDT
     C                   Z-ADD     CSSIC         SCSIC
     C                   Z-ADD     CSAREA        SCAREA
     C***** BEGIN ADD  ****  11/11/97  ********************** PART 4 **
     C                   Z-ADD     CSDEDT        SCDEDT
     C***** END   ADD  ****  11/11/97  ********************** PART 4 **
     C***** BEGIN DELETE **  11/11/97  ********************** PART 3 **
     C*******              Z-ADDCSSBBT    SCSBBT
     C*******              Z-ADDCSDLCD    SCDLCD
     C***** END   DELETE **  11/11/97  ********************** PART 3 **
     C                   MOVEL     CSEMNM        SCEMNC
     C*
     C*** CHAIN TO WORKFILE, IF IT EXISTS FILL SELECTION FIELD WITH A '1'
     C*** AND PROTECT IT.
     C*** ALSO, RE-ASSIGN THE WORKFILE'S SUBFILE RRN3, IN CASE THEY'VE
     C*** REORDERED THE SEARCH AND THE SUBFILE RRN HAS CHANGED
     C                   MOVEL     SCSS#         XXSS#
     C*** BEGIN CHG *** 02/10/98 **************************************
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
     C*** END   CHG *** 02/10/98 **************************************
     C     CSKEY4        CHAIN     PCS04                              70
     C     *IN70         IFEQ      '1'
     C                   MOVE      '0'           *IN21
     C                   MOVE      '0'           *IN23
     C                   MOVE      *BLANK        SCSEL3
     C                   END                                                    END 1 *IN70
     C     *IN70         DOWEQ     '0'
     C*** ONLY WANT TO WORK WITH RECORDS THAT ARE NOT CROSS-REFERENCED
     C*** TO A PREVIOUS CLIENT SUMMARY RECORD
     C     CSXREF        IFEQ      *ZERO
     C                   MOVE      '1'           *IN21
     C                   MOVE      '0'           *IN23
     C                   MOVE      '1'           SCSEL3
     C                   Z-ADD     RRN3          CSRRN3
     C                   UPDATE    PCS04
     C                   ELSE                                                   ELSE 0 CSXREF
     C*** IF FROM A PREVIOUS CLIENT REQUEST ON IN THIS SAME JOB/MEMBER
     C*** THEN PROTECT ITEM OPTION (COLUMN SEPARATORS NONDISPLAYED)
     C                   MOVE      '0'           *IN21
     C                   MOVE      '1'           *IN23
     C                   MOVE      *BLANK        SCSEL3
     C                   END                                                    END 0 CSXREF
     C     CSKEY4        READE     PCS04                                  70
     C                   END                                                    END 0 *IN70
     C*
     C*** IF RECORD READ MATCHES SELECTION CRITERIA THEN HI SELECTION
     C     *IN91         IFEQ      '1'
     C* SS# SELECTION
     C     SCUSS#        IFNE      *BLANKS
     C     SCUSS#        ANDEQ     SCSS#
     C                   MOVE      '1'           *IN22
     C                   ELSE                                                   ELS 1 *IN22
     C                   MOVE      '0'           *IN22
     C                   END                                                    END 1 *IN22
     C*
     C                   ELSE                                                   ELS 1 *IN91
     C*
     C     *IN92         IFEQ      '1'
     C* EMPLOYEE NAME SELECTION
     C*
     C     SCULST        IFNE      *BLANKS
     C     SCULST        ANDEQ     SCLAST
     C                   MOVE      '1'           *IN22
     C                   ELSE                                                   ELS 1 *IN22
     C                   MOVE      '0'           *IN22
     C                   END                                                    END 1 *IN22
     C*
     C                   ELSE                                                   ELS 1 *IN92
     C*
     C     *IN93         IFEQ      '1'
     C* EMPLOYEE NAME SELECTION
     C     SCUEM#        IFNE      *ZEROS
     C     SCUEM#        ANDEQ     SCEMP#
     C                   MOVE      '1'           *IN22
     C                   ELSE                                                   ELS 1 *IN22
     C                   MOVE      '0'           *IN22
     C                   END                                                    END 1 *IN22
     C                   END                                                    ELS 1 *IN93
     C*
     C                   END                                                    END 1 *IN92
     C*
     C                   END                                                    END 1 *IN91
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR03S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN3          IFEQ      0
     C     *IN61         ANDEQ     '1'
     C                   MOVE      *BLANKS       SCSS#
     C                   MOVE      *BLANKS       SCLAST
     C                   MOVE      *BLANKS       SCFRST
     C                   MOVE      *BLANKS       SCMI
     C                   MOVE      *BLANKS       SCACDT
     C                   MOVE      *BLANKS       SCEMP#
     C                   MOVE      *BLANKS       SCEMNM
     C***** BEGIN ADD  ****  11/11/97  ****************** PART 4 ******
     C                   MOVE      *BLANKS       SCPART
     C***** END   ADD  ****  11/11/97  ****************** PART 4 ******
     C***** BEGIN DELETE **  11/11/97  ****************** PART 3 ******
     C*******              MOVE *BLANKS   SCITYP
     C*******              MOVE *BLANKS   SCDLST
     C***** END   DELETE **  11/11/97  ****************** PART 3 ******
     C                   MOVE      *BLANKS       SCDTYP
     C*** HIDDEN SUBFILE FIELDS
     C                   MOVE      *BLANKS       SCDIV
     C                   MOVE      *BLANKS       SCCARR
     C                   MOVE      *BLANKS       SCBTDT
     C                   MOVE      *BLANKS       SCSIC
     C                   MOVE      *BLANKS       SCAREA
     C***** BEGIN ADD  ****  11/11/97  ****************** PART 4 ******
     C                   MOVE      *BLANKS       SCDEDT
     C***** END   ADD  ****  11/11/97  ****************** PART 4 ******
     C***** BEGIN DELETE **  11/11/97  ****************** PART 2 ******
     C*******              MOVE *BLANKS   SCSBBT
     C*******              MOVE *BLANKS   SCDLCD
     C***** END   DELETE **  11/11/97  ****************** PART 2 ******
     C                   MOVE      *BLANKS       SCEMNC
     C*
     C                   Z-ADD     1             RRN3
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR03S
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     RRN3          #@$3
     C*
     C                   MOVEL     *BLANK        SCUSS#
     C                   MOVEL     *BLANK        SCULST
     C                   MOVEL     *BLANK        SCUFRT
     C                   MOVEL     *BLANK        SCUMI
     C                   MOVE      *BLANK        SCUEM#
     C*
     C                   ENDSR
     C*====================================================================*
     C* * *  LOAD4 SUBROUTINE                                    L O A D ***
     C*====================================================================*
     C     LOAD4         BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        13            X
     C*
     C     AGAIN         TAG
     C                   READ      PCS04                                  61
     C*
     C*** ONLY WANT TO WORK WITH RECORDS THAT ARE NOT CROSS-REFERENCED
     C***    TO A PREVIOUS CLIENT SUMMARY RECORD
     C*** IF YOU GET A HIT, BUT THE DETAIL RECORD IS CROSS-REFERENCED
     C***    TO A PREVIOUS CLIENT SUMMARY RECORD, THEN READ THE NEXT RECORD
     C     *IN61         IFEQ      '0'
     C     CSXREF        ANDNE     *ZERO
     C                   GOTO      AGAIN
     C                   END
     C*
     C     *IN61         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN61         IFEQ      '0'
     C                   ADD       1             RRN4
     C                   MOVEL     CSSS#         SCSS#
     C                   MOVEL     CSLAST        SCLAST
     C                   MOVEL     CSFRST        SCFRST
     C                   MOVEL     CSMI          SCMI
     C***** BEGIN ADD  ****  11/11/97  ********************** PART 1 **
     C********             Z-ADDCSACDT    SCACDT
     C                   Z-ADD     CSACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCACDT
     C***** END CHANGE ****  11/11/97  ********************** PART 1 **
     C                   Z-ADD     CSEMP#        SCEMP#
     C                   MOVEL     CSEMNM        SCEMNM
     C***** BEGIN ADD  ****  11/11/97  ********************** PART 3 **
     C                   MOVEL     CSPART        SCPART
     C***** END   ADD  ****  11/11/97  ********************** PART 3 **
     C***** BEGIN DELETE **  11/11/97  ********************** PART 2 **
     C*******              Z-ADDCSITYP    SCITYP
     C*******              Z-ADDCSDLST    SCDLST
     C***** END   DELETE **  11/11/97  ********************** PART 2 **
     C                   Z-ADD     CSDTYP        SCDTYP
     C*** HIDDEN SUBFILE FIELDS
     C                   Z-ADD     CSDIV         SCDIV
     C                   Z-ADD     CSCARR        SCCARR
     C                   Z-ADD     CSBTDT        SCBTDT
     C                   Z-ADD     CSSIC         SCSIC
     C                   Z-ADD     CSAREA        SCAREA
     C***** BEGIN ADD  ****  11/11/97  ********************** PART 4 **
     C                   Z-ADD     CSDEDT        SCDEDT
     C***** END   ADD  ****  11/11/97  ********************** PART 4 **
     C***** BEGIN DELETE **  11/11/97  ********************** PART 3 **
     C*******              Z-ADDCSSBBT    SCSBBT
     C*******              Z-ADDCSDLCD    SCDLCD
     C***** END   DELETE **  11/11/97  ********************** PART 3 **
     C                   MOVEL     CSEMNM        SCEMNC
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR04S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END                                                    END DO
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN4          IFEQ      0
     C     *IN61         ANDEQ     '1'
     C                   MOVE      *BLANKS       SCSS#
     C                   MOVE      *BLANKS       SCLAST
     C                   MOVE      *BLANKS       SCFRST
     C                   MOVE      *BLANKS       SCMI
     C                   MOVE      *BLANKS       SCACDT
     C                   MOVE      *BLANKS       SCEMP#
     C                   MOVE      *BLANKS       SCEMNM
     C***** BEGIN ADD  ****  11/11/97  ********************** PART 4 **
     C                   MOVE      *BLANKS       SCPART
     C***** END   ADD  ****  11/11/97  ********************** PART 4 **
     C***** BEGIN DELETE **  11/11/97  ********************** PART 2 **
     C*******              MOVE *BLANKS   SCITYP
     C*******              MOVE *BLANKS   SCDLST
     C***** END   DELETE **  11/11/97  ********************** PART 2 **
     C                   MOVE      *BLANKS       SCDTYP
     C*** HIDDEN SUBFILE FIELDS
     C                   MOVE      *BLANKS       SCDIV
     C                   MOVE      *BLANKS       SCCARR
     C                   MOVE      *BLANKS       SCBTDT
     C                   MOVE      *BLANKS       SCSIC
     C                   MOVE      *BLANKS       SCAREA
     C***** BEGIN ADD  ****  11/11/97  ********************* PART 4 ***
     C                   MOVE      *BLANKS       SCDEDT
     C***** END   ADD  ****  11/11/97  ********************* PART 4 ***
     C***** BEGIN DELETE **  11/11/97  ********************* PART 2 ***
     C*******              MOVE *BLANKS   SCSBBT
     C*******              MOVE *BLANKS   SCDLCD
     C***** END   DELETE **  11/11/97  ********************* PART 2 ***
     C                   MOVE      *BLANKS       SCEMNC
     C*
     C                   Z-ADD     1             RRN4
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR04S
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     RRN4          #@$4
     C*
     C                   ENDSR
      *====================================================================*
      * * *   WRDET SUBROUTINE - WRITE DETAIL TO WORK FILE     W R D E T ***
      *====================================================================*
     C     WRDET         BEGSR
     C*
     C                   MOVEL     SCSS#         XXSS#
     C*** BEGIN CHG *** 02/10/98 **************************************
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CSACDT
     C*** END   CHG *** 02/10/98 **************************************
     C     CSKEY4        CHAIN     PCS04                              62
     C     *IN62         IFEQ      '1'
     C                   MOVEL     SCSS#         CSSS#
     C                   MOVEL     SCLAST        CSLAST
     C                   MOVEL     SCFRST        CSFRST
     C                   MOVEL     SCMI          CSMI
     C*** BEGIN CHG *** 02/10/98 **************************************
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CSACDT
     C*** END   CHG *** 02/10/98 **************************************
     C                   Z-ADD     SCEMP#        CSEMP#
     C                   MOVEL     SCEMNM        CSEMNM
     C***** BEGIN ADD  ****  11/11/97  ******************* PART 4 *****
     C                   MOVEL     SCPART        CSPART
     C***** END   ADD  ****  11/11/97  ******************* PART 4 *****
     C***** BEGIN DELETE **  11/11/97  ******************* PART 2 *****
     C*******              Z-ADDSCITYP    CSITYP
     C*******              Z-ADDSCDLST    CSDLST
     C***** END   DELETE **  11/11/97  ******************* PART 2 *****
     C                   Z-ADD     SCDTYP        CSDTYP
     C*** HIDDEN SUBFILE FIELDS
     C                   Z-ADD     SCDIV         CSDIV
     C                   Z-ADD     SCCARR        CSCARR
     C                   Z-ADD     SCBTDT        CSBTDT
     C                   Z-ADD     SCSIC         CSSIC
     C                   Z-ADD     SCAREA        CSAREA
     C***** BEGIN ADD  ****  11/11/97  ******************* PART 4 *****
     C                   Z-ADD     SCDEDT        CSDEDT
     C***** END   ADD  ****  11/11/97  ******************* PART 4 *****
     C***** BEGIN DELETE **  11/11/97  ******************* PART 3 *****
     C*******              Z-ADDSCSBBT    CSSBBT
     C*******              Z-ADDSCDLCD    CSDLCD
     C***** END   DELETE **  11/11/97  ******************* PART 3 *****
     C                   MOVEL     SCEMNC        CSEMNM
     C*
     C                   Z-ADD     RRN3          CSRRN3
     C                   Z-ADD     *ZERO         CSXREF
     C                   WRITE     PCS04
     C                   ADD       1             SCITMS
     C                   END                                                    END 1 *IN62
     C*
     C                   ENDSR
      *====================================================================*
      * * *   WRSUM SUBROUTINE - WRITE SUMMARY TO CLT FILE     W R S U M ***
      *====================================================================*
     C     WRSUM         BEGSR
     C*
     C                   MOVEL     SCCLT#        CLCLT#
     C                   MOVEL     SCMTF         CLMTF
     C                   Z-ADD     SCITMS        CLITMS
     C*
     C                   MOVE      *BLANKS       CLSS#
     C                   MOVE      *BLANKS       CLLAST
     C                   MOVE      *BLANKS       CLFRST
     C                   MOVE      *BLANKS       CLMI
     C*
     C*** LOAD SS# AND NAME FROM INITIAL EMPLOYEE SELECTION SCREEN SCR02
     C                   MOVEL     SCRSS#        CLSS#                          SCR02 SS#
     C                   MOVEL     SCRLST        CLLAST
     C                   MOVEL     SCRFRT        CLFRST
     C                   MOVEL     SCRMI         CLMI
     C*
     C*** IF THERE WAS AN ITEM SELECTED, THEN USE NAME FROM FIRST ITEM
     C     1             CHAIN     SCR04S                             62
     C     *IN62         IFEQ      '0'
     C     SCLAST        ANDNE     *BLANKS
     C                   MOVEL     SCLAST        CLLAST
     C                   MOVEL     SCFRST        CLFRST
     C                   MOVEL     SCMI          CLMI
     C                   END                                                    END 0 *IN62
     C*
     C*** NEED TO XREF CLIENT SUMMARY WITH EMPLOYEE DETAIL RECORDS
     C***      SO THAT THESE EMPLOYEE DETAIL RECORDS ARE NOT 'REUSED'
     C***      ON ANOTHER SEARCH REQUEST AND SO THAT THE PROPER RECORDS
     C***      ARE PRINTED ON THE SEARCH REQUEST REPORTS
     C*** GET XREF#, UPDATE CLIENT SUMMARY AND EMPLOYEE DETAIL RECORDS
     C     *LOVAL        SETLL     PXR01
     C                   READ      PXR01                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVE      XRXREF        CLXREF
     C                   ADD       1             CLXREF
     C     CLXREF        IFEQ      *ZERO
     C                   ADD       1             CLXREF
     C                   END
     C                   MOVE      CLXREF        XRXREF
     C                   UPDATE    PXR01
     C                   END
     C*
     C     *LOVAL        SETLL     PCS04
     C                   READ      PCS04                                  60
     C     *IN60         DOWEQ     '0'
     C     CSXREF        IFEQ      *ZERO
     C                   Z-ADD     CLXREF        CSXREF
     C                   UPDATE    PCS04
     C                   END                                                    END 0 CSXREF
     C                   READ      PCS04                                  60
     C                   END                                                    END 0 *IN60
     C*
     C                   MOVE      *BLANKS       CLSTAT
     C                   MOVE      Q1USER        CLUSER
     C                   MOVE      Q1PGM         CLPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CLDT
     C*
     C                   Z-ADD     DSTIME        CLTIME
     C                   MOVE      Q1USER        CLIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CLIDT
     C*
     C                   WRITE     PCL01
     C*
     C                   ENDSR
** MSG1 - SCR01 MESSAGE
CLIENT HAS RECEIVED A 2ND BILLING NOTICE, LAST PAID DATE WAS
** MSG4 - SCR04 MESSAGE
*DELETED
