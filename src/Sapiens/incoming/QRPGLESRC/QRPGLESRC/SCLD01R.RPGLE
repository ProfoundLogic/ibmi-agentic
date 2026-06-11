     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SCLD01R - REMARKS/CUSTOM DATA FIELDS - WC FUND
     H*****************************************************************
     H*
     H******************************************************************
     H** REVISIONS:
     H*  04/25/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H*  07/21/98 C:69 Support: 12666
     H*                Changed the way the check for a recursive call
     H*                is done
     H*                                                               *
     H*  02/23/98 C:85 Project: WC577                                 *
     H*                CHANGED THE FIELD MOVED TO CMSMCD              *
     H*                FROM *BLANKS TO CTSMCD.                        *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H******************************************************************
      *****************************************************************
      * FILE SPECIFICATIONS  * * *
      *****************************************************************
      * INDICATOR USAGE
      * SCR01:
      *  03       Office NOT installed: Nondisplay of F1=OV/400
      * N03       Office IS installed:  Display/Use of F1 = OV/400
      *  06      NON DISPLAY ON DOC ID/TYPE/DATE HEADINGS
      *  07      GLOBAL - COMMAND KEY REQUESTED
      *  21      SCM0531-UNABLE TO PROCESS FOR THIS FOLDER
      *  22      SCM0545-NOT ABLE TO BUILD FOLDER (AUTO-CREATE)
      *  23      SCM0546-NOT ABLE TO BUILD DOCUMENT (AUTO-CREATE)
      *  24      SCM0547-REMARK TYPE MUST BE VALID FOR AUTO-CREATE
      *  25      SCM0548-SET FOLDER/DOC INVALID FOR INCOMING PROGRAM
      *  26      SCM0609-DOCUMENT ID IS NOT ALLOWED WITH A REMARK TYPE
      *          THAT HAS AN ASSOCIATED DATA ENTRY PROGRAM ID.
      *  27      SCM0   -NO SUB-AREA EXISTS FOR THIS REMARK TYPE.
      *  31      GLOBAL - SCM0550-AS/400 Officevision is not installed
      *  32      SCM0532-Default Folder required if Document ID indicated
      *  33             -ERROR OCCURED ON SCAN OPERATION
      *  34      GLOBAL - DATA ENTRY PROGRAM OPTION IS INSTALLED
      *  35      SCM0502-VALUE MUST BE 'Y' OR 'N'
      *  37      GLOBAL - Used on call to Data Entry Program (DEPGM)
      *  42      SCM0603-VALUE MUST BE 'Y', 'N', OR BLANK.
      *  43      SCM0604-THIS REMARK TYPE IS NON-PURGEABLE.
      *  44      SCM0605-DIARY TYPE MUST BE 'Y' IN ORDER FOR THIS
      *          REMARK TO BE PURGEABLE.
      *  46      SCM0502-
      *  47    SCM0619-Must find alternative access to this data entry*
      *                program.                                       *
      *  48    SCM0620-Data entry program is not available at this    *
      *                time.                                          *
      *  51      SCM0501-IF ENTERED, VALUE ENTERED IS NOT DEFINED IN TABLE FILE
      *  52      SCM0500-INVALID DATE ENTERED
      *  53      SCM0503-VALUE CANNOT BE EQUAL TO BLANKS - REMARK REQUIRED
      *  54      SCM0516-IF ENTERED, DOCUMENT NOT FOUND IN SPECIFIED FOLDER
      *  55      SCM0512-INVALID REMARK CODE ENTERED
      *  56      DISPLAY "UNSECURE REMARK", REMARK IS SECURED
      * N56      DISPLAY "SECURE REMARK", REMARK IS UNSECURED
      *  57      SCM0532-FOLDER MUST BE ENTERED IF DOCUMENT IS ENTERED
      *  58      SCM0612-User is not enrolled in DARTS. See your
      *          System Administrator.
      *  59      VALIDATE CHAIN COMMAND MATCH FOR DEFAULT SECURITY LVL.
      *  60      GLOBAL - GENERAL CHAIN: PSL01, PCM01, PCG01           EDIT PGM
      *  91      SCM0525-DOCUMENT NAME MUST BE VALID TO PERFORM ADD FUNCTION
      *  92      SCM0517-NOT AUTHORIZED TO DOCUMENT OR FOLDER
      *  93      SCM0518-USER NOT ENROLLED IN OFFICE 400
      *  94      SCM0519-UNABLE TO PROCESS REQUEST FOR THIS DOCUMENT
      *  95      SCM0520-DOCUMENT NAME ALREADY EXISTS AS DOCUMENT OR FOLDER
      *  96      SCM0521-FOLDER NOT FOUND
      *  97      GLOBAL - GENERAL CHAIN: PCM0A
      * N98      EITHER NO SECURITY OR MEETS SECURITY LEVEL
      *  98      PROTECT REMARK TYPE BECAUSE OF SECURITY LEVEL
      *  99      GLOBAL - IMAGE PRODUCT IS INSTALLED
      *****************************************************************
      *
     FSCLD01F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSAIFL     IF   E           K DISK
     F*** BEGIN DEL *** 04/25/00 **************************************
     F********SDCML   O   E                    DISK
     F*** END DEL *** 04/25/00 ****************************************
     FSMSLL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F*** BEGIN DEL *** 04/25/00 **************************************
     F********SDCMLI  IF  E           K        DISK
     F********    PCM01                             KRENAMEPCM0I
     F*** END DEL *** 04/25/00 ****************************************
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
      *
      *****************************************************************
      * EXCEPTION SPECIFICATIONS * * *
      *****************************************************************
      *

     D*
     D CK1             S             70    DIM(4) CTDATA PERRCD(1)              COMMAND KEYS - SCR01
     D*
     D*** BEGIN DEL *** 04/25/00 **************************************
     D********PCM0I
     D********      CMKEY                           PIKEY
     D********      CMSEQ#                          PISEQ#
     D*** END DEL *** 04/25/00 ****************************************
      *****************************************************************
      * INPUT SPECIFICATIONS  * * *
      *****************************************************************
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
      * * *  FILE INFORMATION DATA STRUCTURE
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************
      *

     C***** DELETE THESE ***********************************
     C                   MOVE      Q2RECF        XXRECF           10            *
     C                   MOVEL     'QHLP    '    FILNAM           10            *
     C                   MOVEL     '*LIBL   '    LIBNAM           10            *
     C                   MOVE      XXSCR#        MBRNAM           10            *
      ******************************************************
     C*
     C                   Z-ADD     0             C                 5 0
     C                   Z-ADD     0             C1                5 0
     C                   MOVE      'N'           XXDEWR            1            ADD 04/09/92
     C                   MOVE      *BLANKS       DERTCD            1            ADD 04/09/92
     C                   MOVE      *BLANKS       DECOM1          256            ADD 04/09/92
     C                   MOVE      *BLANKS       DECOM2           19            ADD 04/09/92
     C*
     C                   MOVE      *BLANKS       DEKEY                          ADD 04/09/92
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND             3 0
     C*
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*
     C*   KEY LIST FOR THE REMARKS DETAIL RECORD
     C*
     C*** BEGIN DEL *** 04/25/00 **************************************
     C********   CMKYA     KLIST
     C********             KFLD           QQKEY
     C********             KFLD           ##SEQ#  50
     C********
     C********
     C****KEY*LIST FOR THE REMARKS DETAIL RECORD
     C********
     C********   CMKY      KLIST
     C********             KFLD           QQKEY
     C********
     C********   CMKY1     KLIST
     C********             KFLD           QQKEY
     C********             KFLD           XXDATE
     C********             KFLD           XXTIME
     C********             KFLD           XXSEQ#
     C********
     C********   CMKY2     KLIST
     C********             KFLD           QQKEY
     C********             KFLD           @@CDAT  70
     C*** END DEL *** 04/25/00 ****************************************
     C*
     C*
     C*   KEY LIST FOR THE SECURITY FILE
     C*
     C     SLKY          KLIST
     C                   KFLD                    $$USER
     C*
     C*
     C*
     C*
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*   KEY LIST FOR THE ADDITIONAL COMMENTS FILE
     C*
     C*** BEGIN DEL *** 04/25/00 **************************************
     C********   ARKY      KLIST
     C********             KFLD           CMKEY
     C********             KFLD           CMSEQ#
     C********
     C****KEY*LIST FOR THE REMARK SUMMARY CODE FILE
     C********
     C****KEY*LIST FOR THE REMARK TYPE FILE
     C********
     C********   CTKY      KLIST
     C********             KFLD           CMRMCD
     C*** END DEL *** 04/25/00 ****************************************
     C*
     C     CTKY1         KLIST
     C                   KFLD                    SCCODE            5
     C*** FAX INCOMING FILE ****
     C     IFKY          KLIST
     C                   KFLD                    QQFMBR
     C*
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C*** BEGIN DEL *** 04/25/00 **************************************
     C********   *LIKE     DEFN CMCDAT    XXDATE
     C********   *LIKE     DEFN CMTRTM    XXTIME
     C********   *LIKE     DEFN CMSEQ#    XXSEQ#
     C*** END DEL *** 04/25/00 ****************************************
     C     *LIKE         DEFINE    DRAL02        OFCPGM
     C     *LIKE         DEFINE    Q2RECF        HLRECF
     C*** BEGIN CHG *** 04/25/00 **************************************
     C********   *LIKE     DEFN CMKEY     DEKEY
     C********   *LIKE     DEFN CMCDAT    XXCDAT
     C     *LIKE         DEFINE    CODT          XXCDAT
     C********   *LIKE     DEFN CMCDAT    XXDSDT
     C     *LIKE         DEFINE    CODT          XXDSDT
     C********   *LIKE     DEFN CMCDAT    HHDT
     C*** END CHG *** 04/25/00 ****************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFMBR           10
     C                   PARM                    QQRCOD            1
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
     C                   MOVEL     'SC'          XXSYK
     C*
     C     IFKY          CHAIN     PIF01                              60
     C*** END IF NO FAX RECORD *** ERROR!!!!!
     C     *IN60         CABEQ     '1'           TERM
     C*
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
     C*
     C/EJECT
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C     *IN07         IFEQ      '1'
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVEL     '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*   F5 = EMPLOYER - NUMBER SEARCH      *
     C****************************************
     C     *INKE         IFEQ      '1'
     C                   MOVEL     SCCO#         @@CO#
     C                   CALL      'W0010R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM      1             @@VIEW            1 0
     C*
     C                   MOVE      @@FND         SCFND
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      'B'
     C     SCFND         OREQ      *ZERO
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ELSE
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C****************************************
     C*  F9 = USER PROFILE SEARCH           *
     C****************************************
     C     *INKI         IFEQ      '1'
     C     SLF09         IFEQ      'Y'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   CALL      'SC530R'
     C                   PARM                    SCDUSR           10
     C                   PARM                    QQRTCD            1
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END SCR01
     C*
     C                   END                                                    END SLF09=Y
     C                   GOTO      STRT
     C                   END                                                    END *INKI
     C****************************************
     C*  F10 = TABLE FILE SEARCH             *
     C****************************************
     C     *INKJ         IFEQ      '1'
     C     SLF10         IFEQ      'Y'
     C***********
     C*  SCR01  *
     C***********
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C     SCDEPT        IFEQ      '? '
     C                   MOVEL     *BLANKS       SCCODE
     C*
     C                   MOVE      *BLANK        QQRTCD
     C                   CALL      'SC580R'
     C                   PARM                    SCCODE
     C                   PARM                    QQRTCD
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVEL     SCCODE        SCDEPT
     C                   MOVE      SCCODE        SCRMTY
     C     CTKY1         CHAIN     PCT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CTFL01        SCDITP
     C                   END
     C                   GOTO      STRT
     C                   END                                                    SCCODE ='? '
     C*
     C                   MOVEL     SCPNAM        $$2A              2
     C     $$2A          IFEQ      '? '
     C                   MOVEL     *BLANKS       SCPNAM
     C*
     C                   MOVE      *BLANK        QQRTCD
     C                   CALL      'SC590R'
     C                   PARM                    SCPNAM
     C                   PARM                    QQRTCD
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    SCPNAM ='? '
     C*
     C     SCRMTY        IFEQ      '? '
     C                   MOVEL     *BLANKS       SCCODE
     C                   MOVEL     SCDEPT        SCCODE
     C                   MOVE      *BLANK        QQRTCD
     C                   CALL      'SC580R'
     C                   PARM                    SCCODE
     C                   PARM                    QQRTCD
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVEL     SCCODE        SCDEPT
     C                   MOVE      SCCODE        SCRMTY
     C     CTKY1         CHAIN     PCT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CTFL01        SCDITP
     C                   END
     C                   GOTO      STRT
     C                   END                                                    SCCODE ='? '
     C*
     C                   END                                                    END SCR01
     C*
     C*
     C                   END                                                    END SLF10=Y
     C                   GOTO      STRT
     C                   END                                                    END *INKJ
     C****************************************
     C*   F12 = PREVIOUS SCREEN(CANCEL)      *
     C****************************************
     C     *INKL         IFEQ      '1'
     C     SLF12         IFEQ      'Y'
     C***********
     C*  SCR01  *
     C***********
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C                   END                                                    END SLF12=Y
     C                   GOTO      STRT
     C                   END                                                    END *INKL
     C*****************************************************************
     C** INKM - FUNCTION 13 - CHANGE USER PARMS
     C*****************************************************************
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         XXCO#
     C                   MOVE      *ZERO         SCCO#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C****************************************
     C*   F23=SECURE REMARKS  BY LEVEL       *
     C****************************************
     C     *INKX         IFEQ      '1'
     C     SLF23         IFEQ      'Y'
     C***********
     C*  SCR01  *
     C***********
     C*  Remark UNSECURED to SECURED
     C     Q2RECF        IFEQ      'SCR01'
     C     *IN56         IFEQ      '0'
     C                   MOVEL     *BLANKS       SCCODE
     C                   MOVEL     SCDEPT        SCCODE
     C                   MOVE      SCRMTY        SCCODE
     C     CTKY1         CHAIN     PCT01                              59        +DEFAULT SECLVL
     C     *IN59         IFEQ      '1'
     C*** ??? ** CTDSLV    OREQ *ZEROS
     C                   MOVE      SLSECL        SCSECL
     C                   ELSE
     C*** ??? ***          MOVE CTDSLV    SCSECL
     C                   END
     C                   MOVE      '1'           *IN56
     C                   MOVE      SCSECL        ##SECL            3 0
     C                   MOVE      'Y'           ##SBYL            1
     C*** DISPLAY "UNSECURE REMARKS", IF "SECURE REMARKS" WAS DISPLAYED
     C     C1            IFEQ      3
     C                   Z-ADD     4             C1
     C                   END
     C*
     C                   ELSE
     C*  Remark SECURED to UNSECURED
     C                   MOVE      '0'           *IN56
     C                   MOVE      *ZEROS        SCSECL
     C                   MOVE      SCSECL        ##SECL            3 0
     C                   MOVE      'N'           ##SBYL            1
     C*** DISPLAY "SECURE REMARKS", IF "UNSECURE REMARKS" WAS DISPLAYED
     C     C1            IFEQ      4
     C                   Z-ADD     3             C1
     C                   END
     C                   END
     C                   MOVEL     CK1(C1)       SCCML1                         ADD 04/06/92
     C*
     C                   END
     C*
     C                   END                                                    END SLF23=Y
     C                   GOTO      STRT
     C                   END                                                    END *INKX
     C****************************************
     C*   F24=More Keys                      *
     C****************************************
     C     *INKY         IFEQ      '1'
     C     SLF24         IFEQ      'Y'
     C***********
     C*  SCR01  *
     C***********
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C     C1            IFEQ      1
     C                   Z-ADD     2             C1
     C                   GOTO      CMDK#
     C                   END
     C*  Remark Secured
     C     *IN56         IFEQ      '1'
     C     C1            ANDEQ     2
     C                   Z-ADD     4             C1
     C                   GOTO      CMDK#
     C                   END
     C*
     C*  Remark NOT Secured
     C     *IN56         IFEQ      '0'
     C     C1            ANDEQ     2
     C                   Z-ADD     3             C1
     C                   GOTO      CMDK#
     C                   END
     C*
     C     C1            IFEQ      3
     C     C1            OREQ      4
     C                   Z-ADD     1             C1
     C                   GOTO      CMDK#
     C                   END
     C*
     C*
     C     CMDK#         TAG
     C*
     C                   MOVEL     CK1(C1)       SCCML1
     C*
     C                   END                                                    END SCR01
     C                   END                                                    END S24
     C                   GOTO      STRT
     C                   END                                                    END *INKY
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11/CMDKEYS
     C*
      *****************************************************************
      *   GOTO THE APPROPRIATE SCREEN
      *****************************************************************
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUESTED SCREEN
     C*
      *------------------------*
     C     SCR01T        TAG
      *------------------------*
     C*
     C                   MOVE      SCCO#         ##CO#             3
     C                   MOVE      SCFND         ##FND             3
     C*
     C     ##CO#         CAT(P)    ##FND:0       SCKEY            40
     C                   EXSR      EDTS01                                        EDIT SCR01 FIELDS
     C*
     C*** WRITE REMARK RECORD
     C*
     C*  *** MOVE SCREEN KEY INTO CM RCD
     C*****DEL***40        SUBSTSCKEY:1   ##KEY  40
     C                   MOVE      SCCO#         ##CO#             3
     C                   MOVE      SCFND         ##FND             3
     C*
     C     ##CO#         CAT(P)    ##FND:0       ##KEY            40
     C*
     C*  *** PROGRAM NAME TO USE FOR PNAM - KEY STRUCTURE ?
     C                   MOVEL     'W0010R  '    ##PNAM           10
     C*
     C                   EXSR      WCOM
     C*
     C*
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01 '      Q2RECF
     C*
     C*
      *------------------------*
     C     TERM          TAG
      *------------------------*
     C*
     C                   MOVE      '1'           *INLR
     C*
     C/EJECT
     C*
      *====================================================================*
      * * *    NIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C*
     C     INIT          BEGSR
     C                   TIME                    DSDTTM
     C                   MOVE      '0'           XXOPN             1
     C                   MOVE      '-'           XXHYP1            1
     C                   MOVE      '-'           XXHYP2            1
     C                   MOVE      *HIVAL        @@TIME            6 0
     C                   Z-ADD     0             RRN2              5 0
     C                   Z-ADD     0             RRN3              5 0
     C                   Z-ADD     0             RRN6              5 0
     C                   Z-ADD     0             XRRN2             5 0
     C                   Z-ADD     0             XRRN6             5 0
     C                   Z-ADD     0             XXRRN2            5 0
     C                   Z-ADD     0             XXRRN6            5 0
     C                   Z-ADD     0             HLDRRN            5 0
     C                   Z-ADD     0             HL3RRN            5 0
     C                   Z-ADD     0             HLDRR2            5 0
     C                   Z-ADD     0             HLDRR3            5 0
     C*** BEGIN DEL *** 04/25/00 **************************************
     C********             Z-ADD0         XXTIME
     C********             Z-ADD0         XXDATE
     C********             Z-ADD0         XXSEQ#
     C********             Z-ADD*HIVAL    ##SEQ#
     C*** END DEL *** 04/25/00 ****************************************
     C                   Z-ADD     0             ##SECL
     C                   MOVE      'N'           ##SBYL
     C*** GET USER PARM
     C     UPKEY         CHAIN     PUP01                              60
     C                   MOVE      UPCO#         XXCO#             3 0
     C                   MOVE      UPFND         XXFND             3 0
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         XXCO#
     C                   MOVE      *ZERO         XXFND
     C                   MOVE      *ZEROS        SCCO#
     C                   MOVE      *ZEROS        SCFND
     C                   END
     C*
     C*
     C*  GET THE COMPANY AND FUND RECORDS
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CONAME        $$CNAM           40
     C                   MOVE      XXCO#         SCCO#
     C                   ELSE
     C                   MOVEL     *BLANKS       $$CNAM
     C                   MOVE      *ZEROS        SCCO#
     C                   ENDIF
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      XXFND         SCFND
     C                   MOVEL     FNNAME        $$FNAM           40
     C                   ELSE
     C                   MOVE      *ZERO         SCFND
     C                   MOVE      *BLANKS       $$FNAM
     C                   END
     C*
     C*
     C                   MOVEL     Q1USER        $$USER           10
     C     SLKY          CHAIN     PSL01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     *BLANKS       $$USER           10
     C     SLKY          CHAIN     PSL01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     *BLANK        Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   MOVEL     SLFOLD        SCDFLR           80
     C*
     C     SLCCIF        IFNE      'Y'
     C                   MOVE      '1'           *IN98
     C                   ELSE
     C                   MOVE      '0'           *IN98
     C                   END
     C*
     C*** Check System Control File Install Flags for this Client:
     C     'SY'          CHAIN     SSY01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   Check for AS/400 Office Installed
     C     SYOFIF        IFEQ      'Y'
     C                   MOVE      'Y'           XXOFIF            1            Installed
     C                   MOVE      '0'           *IN03                          ADD 04/06/92
     C                   ELSE                                                   ELS 'Y' SYOFIF
     C                   MOVE      'N'           XXOFIF                         Installed
     C                   MOVE      '1'           *IN03                          ADD 04/06/92
     C                   END                                                    END 'Y' SYOFIF
     C*
     C                   END                                                    END 0 *IN60
     C*
     C* LOOK AT DARTS SYSTEM CONTROL FILE TO DETERMINE WHAT'S INSTALLED
     C     SYKY          CHAIN     SDR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*** IF IMAGE IS INSTALLED, SET ON IMAGE INDICATOR.
     C     DRFL01        IFEQ      'Y'
     C                   MOVE      '1'           *IN99
     C                   END
     C*
     C*      (DRFL03 = 'Y', *IN34 = 1)
     C     DRFL03        IFEQ      'Y'                                          ADD 04/09/92
     C                   MOVE      '1'           *IN34
     C                   END                                                    ADD 04/09/92
     C*
     C*** IF SUMMARY CODE  OPTION IS INSTALLED, SET ON INDICATOR.
     C*      (DRFL02 = 'Y', *IN36 = 1), INITIALIZE        TO *BLANKS
     C     DRFL02        IFEQ      'Y'                                          ADD 04/21/92
     C                   MOVE      '1'           *IN36                          ADD 04/21/92
     C                   END                                                    ADD 04/21/92
     C*
     C                   END                                                    END 0 *IN60
     C*
     C*
     C                   Z-ADD     1             C1
     C*
     C                   MOVEL     CK1(C1)       SCCML1
     C*
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   ENDSR
     C*
      *====================================================================*
      * * *   WCOM  SUBROUTINE - WRITE REMARK RECORD           W C O M   ***
      *====================================================================*
     C*
     C     WCOM          BEGSR
     C*** BEGIN DEL *** 04/25/00 **************************************
     C********             TIME           DSDTTM
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     HHDT
     C********
     C********             Z-ADDDSTIME    HHTIME  60
     C********
     C****GET*LAST SEQ# USED
     C********
     C******WRITE REMARKS  TO FILE * * *
     C********
     C********             MOVE ##SECL    CMSECL
     C********             MOVE ##SBYL    CMSBYL
     C********
     C********             MOVEL##KEY     CMKEY
     C********             MOVEL*BLANKS   CMCSID           ANY LONGER
     C********             MOVEL##PNAM    CMPNAM
     C********             MOVE *BLANKS   CMCODE
     C********             MOVELSCCODE    CMRMCD
     C********                                                         *********
     C****BEGIN ADD *** 02/23/98 ***********************************************
     C********                                                         *********
     C********   CMRMCD    CHAINPCT01                60
     C********                                                         *********
     C********   *IN60     IFEQ *OFF
     C********             MOVE CTSMCD    CMSMCD
     C********             ENDIF                           *IN60=OFF
     C********                                                         *********
     C****END*  ADD *** 02/23/98 ***********************************************
     C********
     C********             MOVELIFDCID    CMDCID
     C********             MOVELSCCOMM    CMCOMM
     C********             MOVE ' '       CMFL01
     C*******LOAD FAX FOLDER HERE *********************
     C********             MOVELIFFOLD    CMFOLD
     C********
     C********             Z-ADDSCCDAT    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMCDAT
     C********
     C********             Z-ADDHHDT      CMTRDT
     C********             Z-ADDHHTIME    CMTRTM
     C********
     C********
     C********
     C******FILL HIDDEN FIELDS ****
     C********
     C******IMAGE FIELDS
     C********             MOVELSCIQUE    CMIQUE               HIDDEN
     C********             MOVELSCIPTH    CMIPTH               HIDDEN
     C********             MOVE SCPIDX    CMPIDX               HIDDEN
     C********             MOVELSCAPID    CMAPID               HIDDEN
     C********             MOVELSCIDOC    CMIDOC               HIDDEN
     C********             MOVELSCQPTY    CMQPTY               HIDDEN
     C********             MOVELSCDCTP    CMDCTP               HIDDEN
     C********             MOVEL'F'       CMIMGF               HIDDEN
     C********
     C********
     C********             MOVEL*BLANK    CMSTAT
     C********             MOVELQ1PGM     CMPGM
     C********
     C****IF*DIARY USER ENTERED, WRITE TO CMUSER
     C********   SCDUSR    IFNE *BLANKS
     C********             MOVELSCDUSR    CMUSER
     C********             ELSE                            ELSE NE *BLANKS
     C****ELSE DEFAULT TO CURRENT USER
     C********             MOVELQ1USER    CMUSER
     C********             END                             END NE *BLANKS
     C********
     C********             MOVELQ1USER    CMIUSR
     C********             TIME           DSDTTM
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMDT
     C********
     C********             Z-ADDDSTIME    CMTIME
     C********   SCDITP    IFEQ 'Y'
     C********   SCDITP    OREQ 'N'
     C********
     C********             MOVE SCDITP    IMDIFL
     C********             ELSE
     C********             MOVE ' '       CMDIFL
     C********             END
     C********                                             DFT LOC=DASD
     C********             MOVEL'D'       CMFL05           ADD 05/11/93
     C********
     C********             MOVE SCPGFL    CMFL02           ADD 03/20/92
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMIDT
     C********
     C********             Z-ADD*ZERO     CMDCDT
     C********             MOVELQ1PGM     CMIPGM           ADD 10/14/91
     C********                                                         *********
     C****BEGIN DEL *** 02/23/98 ***********************************************
     C**********           MOVE *BLANKS   CMSMCD
     C****END*  DEL *** 02/23/98 ***********************************************
     C********                                                         *********
     C********             MOVE *BLANKS   CMFL04
     C********             MOVE *BLANKS   CMFL05
     C********
     C****GET*LAST SEQ# USED
     C********
     C********             MOVELSCKEY     QQKEY  40
     C********
     C********   CMKYA     SETGTPCM0I
     C********             READPPCM0I                    60
     C********   *IN60     IFEQ '0'
     C********   QQKEY     ANDEQPIKEY
     C********   PISEQ#    ADD  1         CMSEQ#
     C********             ELSE
     C********             Z-ADD0         CMSEQ#
     C********             END
     C********
     C********             WRITEPCM01
     C******BEGIN ADD **** 07/01/99 ***********************************
     C********             FEOD SDCML
     C******END   ADD **** 07/01/99 ***********************************
     C*** END DEL *** 04/25/00 ****************************************
     C*** BEGIN ADD *** 04/25/00 **************************************
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  ##KEY         Q#KEY            40
     C                   MOVEL(P)  SCCOMM        Q#COMM          256
     C                   MOVE      SCCOMM        Q#COM2           19
     C                   MOVEL(P)  SCCDAT        Q#CDAT            8
     C                   MOVEL(P)  IFDCID        Q#DCID           12
     C                   MOVEL(P)  ##PNAM        Q#PNAM           10
     C                   MOVE      ##SECL        Q#SECL            3
     C     SCDUSR        IFNE      *BLANKS
     C                   MOVEL(P)  SCDUSR        Q#USER           10
     C                   ELSE                                                   DUSR = blanks
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   ENDIF                                                  DUSR = blanks
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  IFFOLD        Q#FOLD           80
     C     SCDITP        IFEQ      'Y'
     C     SCDITP        OREQ      'N'
     C                   MOVE      SCDITP        Q#DIFL            1
     C                   ELSE                                                   DITP = 'Y'
     C                   MOVE      *BLANK        Q#DIFL            1
     C                   ENDIF                                                  DITP = 'Y'
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  SCPGFL        Q#FL02            1
     C                   MOVEL(P)  SCCODE        Q#RMCD            5
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  ##SBYL        Q#SBYL            1
     C                   MOVEL(P)  SCAPID        Q#APID            2
     C                   MOVEL(P)  SCIDOC        Q#IDOC           12
     C                   MOVEL(P)  SCIQUE        Q#IQUE           10
     C                   MOVEL(P)  'F'           Q#IMGF            1
     C                   MOVEL(P)  SCIPTH        Q#IPTH           10
     C                   MOVE      SCPIDX        Q#PIDX            5
     C                   MOVEL(P)  SCQPTY        Q#QPTY            1
     C                   MOVEL(P)  SCDCTP        Q#DCTP            5
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   MOVEL     SCKEY         QQKEY            40
     C                   MOVE      Q#SEQ#        ##SEQ#            5 0
     C*** END ADD *** 04/25/00 ****************************************
     C                   MOVEL     *BLANKS       QQRCOD
     C*
     C*** IF ADDITIONAL RMRKS ARE REQUESTED (SCARMK='Y') THEN CALL PROGRAM T
     C*** TO ADD THE REMARKS.
     C*
     C                   MOVEL     'SCLD01R '    QQPNAM
     C     SCARMK        IFEQ      'Y'
     C                   CALL      'SC014R'
     C                   PARM                    QQKEY
     C*** BEGIN CHG *** 04/25/00 **************************************
     C********             PARM           CMSEQ#
     C                   PARM                    ##SEQ#
     C*** END CHG *** 04/25/00 ****************************************
     C                   PARM                    QQPNAM           10
     C                   END
     C*
     C*
     C*
     C                   ENDSR
     C/EJECT
     C*
      *====================================================================*
      * * *   EDTS01  SUBROUTINE - EDIT SCREEN 1 FIELDS    E D T S 0 1   ***
      *====================================================================*
     C*
     C*
     C     EDTS01        BEGSR
     C*
     C*
     C* VALIDATE EMPLOYER AND DIVISION
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              21
     C     *IN21         CABEQ     '1'           STRT
     C*
     C*** MUST ENTER VALID DATE ***
     C*
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCDAT
     C*
     C     SRERR         CABEQ     '1'           STRT                     52
     C*
     C                   MOVE      ' '           #DIARY            1             DIARY TYPE FLAG
     C*   REMARK TYPE MUST BE ENTERED, AND IT MUST BE VALID
     C                   MOVEL     SCDEPT        SCCODE
     C                   MOVE      SCRMTY        SCCODE
     C*
     C*    If remark is secured, the secuity level displayed on screen
     C*      is moved into the field to be stored when record is made
     C*      or updated.
     C*
     C     *IN56         IFEQ      '1'
     C     SCSECL        ANDNE     ##SECL
     C                   MOVEL     SCSECL        ##SECL
     C                   ELSE
     C     *IN56         IFEQ      '0'
     C                   MOVEL     *ZEROS        SCSECL
     C                   MOVEL     SCSECL        ##SECL
     C                   END
     C                   END
     C     SCCODE        IFNE      *BLANKS
     C     SCCODE        CHAIN     STCTL1                             51
     C     *IN51         CABEQ     '1'           STRT
     C                   ELSE
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C*
     C**** DETERMINE IF REMARK TYPE IS DIARY OR NOT *****
     C***                  ELSE
     C***********          END
     C*
     C                   END                                                     SCCODE NE *BLANKS
     C*
     C     SCDITP        IFEQ      'Y'
     C*
     C*** MUST ENTER A VALID USER NAME
     C*
     C                   MOVEL     SCDUSR        $$USER
     C     SLKY          CHAIN     PSL01                              58
     C     *IN58         CABEQ     '1'           STRT
     C                   END
     C*
     C     SCDITP        IFNE      ' '
     C     SCDITP        IFNE      'Y'
     C     SCDITP        ANDNE     'N'
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   END                                                    END SCDITP NE Y/N
     C                   ELSE                                                   ELSE SCDITP NE ' '
     C                   MOVE      CTFL01        SCDITP
     C                   END                                                    END SCDITP NE ' '
     C*
     C*
     C*  Check Data Entry Flag to see if Data Entry Program has been
     C*     called once already.
     C*
     C     XXDEFL        IFEQ      'N'
     C*  If DRFL03 = 'Y' in file SDRPF, Remark Code is checked to see
     C*     if it has an associated Data Entry Program ID.
     C*
     C                   MOVE      'N'           XXDEPG            1
     C     SYKY          CHAIN     SDR01                              40
     C     *IN40         IFEQ      '0'
     C     DRFL03        IFEQ      'Y'
     C     CTDEPG        IFNE      *BLANKS
     C                   MOVE      'Y'           XXDEPG
     C*
     C     SCDCID        IFNE      *BLANKS
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*  Check to see if a recursive call was made.  If so... error msg
     C*
     C                   MOVE      *BLANKS       SCDCID
     C                   MOVEL     CTDEPG        SCDCID
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDSDT
     C*
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
     C*
     C     XXCDAT        IFLE      XXDSDT
     C                   MOVE      *BLANKS       ##PGM
     C                   CALL      'SRCHKPGM'
     C                   PARM      CTDEPG        ##PGM            10
     C                   PARM                    ##ERR             1
     C*
     C     ##ERR         IFEQ      *ON
     C                   WRITE     SCR99
     C                   WRITE     SCR01
     C                   MOVE      '1'           *IN47
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  ALL EDITS PASSED - CALL DATA ENTRY PROGRAM
     C*  (NOTE: It was necessary to break up the comment field of size
     C*   275 into DECOM1 and DECOM2 because the largest alpha-numeric
     C*   value allowed for a parameter is 256.
     C*
     C*** BEGIN CHG *** 04/25/00 **************************************
     C********             MOVE *BLANKS   DEKEY
     C                   MOVE      *BLANKS       DEKEY            40
     C*** END CHG *** 04/25/00 ****************************************
     C                   MOVE      *BLANKS       DECOM1
     C                   MOVE      *BLANKS       DECOM2
     C                   MOVE      *BLANKS       DERTCD
     C                   MOVEL     QQKEY         DEKEY
     C*
     C                   CALL      CTDEPG                               37
     C                   PARM                    DEKEY
     C                   PARM                    DECOM1
     C                   PARM                    DECOM2
     C                   PARM                    DERTCD
     C*
     C     *IN37         IFEQ      '1'
     C                   EXSR      ERROR
     C                   WRITE     SCR99
     C                   WRITE     SCR01
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   MOVE      'Y'           XXDEFL            1
     C*
     C     DERTCD        IFEQ      'A'
     C     DERTCD        OREQ      'B'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     DERTCD        IFEQ      *BLANKS
     C                   MOVE      'Y'           XXDEWR
     C                   MOVE      *BLANKS       SCCOMM
     C     DECOM1        IFEQ      *BLANK
     C     DECOM2        ANDEQ     *BLANK
     C                   MOVEL     CTDESC        SCCOMM
     C                   MOVEL     CTDESC        DECOM1
     C                   ELSE
     C                   MOVEL     DECOM1        SCCOMM
     C                   MOVE      DECOM2        SCCOMM
     C                   END
     C*
     C                   MOVE      *BLANKS       SCDCID
     C                   MOVEL     CTDEPG        SCDCID
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDSDT
     C*
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
     C*
     C     XXCDAT        IFLE      XXDSDT
     C*
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   END                                                    CTDEPG NE 0
     C                   END                                                    DRFL03 = 'Y'
     C                   END                                                    *IN40 = 0
     C                   END
     C*
     C     *IN99         IFEQ      '0'                                           NO EDIT IF IMAGE
     C     XXDEFL        ANDEQ     'N'
     C*
     C*** IF ENTERED, DOC ID MUST BE VALID (EXISTING)
     C*
     C                   MOVEL     SCDCID        XXDCID
     C                   MOVEL     SCDFLR        XXFOLD           80
     C                   MOVEL     *BLANK        QQRTCD            1
     C*
     C     SCDCID        IFNE      *BLANK
     C*
     C*** If AS/400 Office NOT Installed - Don't allow this function
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*** Must have a Default Folder entered for this function
     C     XXFOLD        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C                   CALL      'SVALDOC'
     C                   PARM                    XXDCID           12
     C                   PARM                    XXFOLD
     C                   PARM                    QQRTCD
     C*
     C     QQRTCD        CABEQ     '1'           STRT                     54
     C*
     C                   END                                                    DCID <> *BLANK
     C*
     C                   END                                                    99 = 0
     C*
     C     SCCOMM        CABEQ     *BLANKS       STRT                     53
     C*
     C*
     C*** MUST ENTER VALID ADDITIONAL REMARKS FLAG
     C     SCARMK        IFNE      'Y'
     C     SCARMK        CABNE     'N'           STRT                 4646
     C                   END
     C*
     C* PURGEABLE ITEM FLAG MUST BE 'Y', 'N', OR BLANK
     C*
     C     SCPGFL        IFNE      'Y'
     C     SCPGFL        ANDNE     'N'
     C     SCPGFL        ANDNE     *BLANK
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C*
     C* IF PURGEABLE ITEM FLAG IS = 'Y'
     C*
     C     SCPGFL        IFEQ      'Y'
     C*
     C     CTDPRG        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN43
     C                   GOTO      STRT
     C                   END
     C*
     C     SCDITP        IFNE      'Y'
     C                   MOVE      '1'           *IN44
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C                   END
     C*
     C* IF PURGEABLE ITEM FLAG IS = *BLANKS
     C*
     C     SCPGFL        IFEQ      *BLANKS
     C*
     C     CTDPRG        IFNE      *ZEROS
     C     SCDITP        ANDEQ     'Y'
     C                   MOVE      'Y'           SCPGFL
     C                   ELSE
     C                   MOVE      'N'           SCPGFL
     C                   END
     C*
     C                   END
     C*
     C*
     C*
     C*
     C                   ENDSR
     C*
      *====================================================================*
      * * *   ERROR SUBROUTINE - EXCEPTION/ERROR SUBROUTINE           * * **
      *====================================================================*
     C     ERROR         BEGSR
     C*
     C*  IF FILE IS NOT FOUND IN USERS LIBL
     C*
     C     Q1STCD        IFEQ      211
     C*
     C                   END
     C*
     C                   ENDSR
     C/EJECT
**  CK1 - Command Keys - SCR01
F3=Exit  F5=Fund Search                   F9=User Search F24=More keys
F10=Table Search   F12=Previous   F13=Chg User Parms     F24=More keys
F23=Secure Remarks Added                                 F24=More keys
F23=Unsecure Remarks Added                               F24=More keys
