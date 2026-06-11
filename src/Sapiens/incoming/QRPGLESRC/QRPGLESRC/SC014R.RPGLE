     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr**** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SC014R - ADDITIONAL REMARKS ENTRY SCREEN
      *****************************************************************
     H** REVISIONS:
     H*
     H*  10/12/15 C:242 PHW37819
     H*                 Removed Compiler director if defined(web)
     H*
     H*  05/27/04 C:15  Support: WC129/42196
     H*                 Added code for a 'Display' only mode.
     H*
     H*
     H*  04/25/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SMARP with API DA701R
     H*
      *  07/23/98  C:33  SUPPORT: DR095/12210
      *                  NEED TO DISPLAY ALL CHARACTERS FOR THE REMARK.
      *
      * * * ----------------------------------------------------- * * *
      *
      *  12/31/91 LJD   - Proj: SC090
      *                   1.  Pass PNAM as a parm into SC014R to update ARPNAM
      *                       with keyed program name
      *
      *  03/23/92 WLD   - Proj: AJP
      *                   1.  Changed Remark Code from 2A to 5A field.  ARPNAM
      *                       (CODE --> RMCD).  Adjusted screen display
      *                       to accommodate new field.
      *
      *  04/06/92 LJD   - Proj: ANI
      *                   1.  Added F1 = OV/400 based upon program name
      *                       in DARTS System Control File SDRPF/DRAL02
      *
      *  04/15/92 WLD   - Proj: ANB
      *                   1.  Based upon the field SDRPF/DRDT01, allow  ARPNAM
      *                       the major remark and additional remarks
      *                       to be updateable until DRDT01 #days after
      *                       each record's initial entry date.
      *
      *  06/17/92 WLD   - Proj: ANB
      *                   1.  Changed number of days to check for
      *                       record to be updateable.
      *                       Used indicators: *IN20, *IN21, *IN22,
      *                       *IN40.
      *
      *  10/26/92 C:10  - Proj: ANB
      *                   1.  Fixed problem where user returns
      *                       from Function 1 - Office Vision and
      *                       main remark becomes 'updateable'.
      *
      *  11/20/92 C:10  - Proj: AQO
      *                   1.  Removed capability to add additional
      *                       lines once the major remark is protected.
      *                       Also, changed code so that all additional
      *                       remarks are protected if the major remark
      *                       is protected.
      *                       Used indicator 20 for additional rmks as
      *                       well as major remark.  Don't need to use
      *                       indicator 21 anymore.  Used indicator
      *                       *IN18 for Roll Up.  Used indicator *IN30
      *                       to allow End of file on roll message to
      *                       display at the correct time.  Used *IN32
      *                       for End of File on Roll message on roll
      *                       up or roll down.
      *                       NOTE: The "Roll up" function that was
      *                             added (*IN18) is only used when the
      *                             major remark is protected;
      *                             otherwise, the "Roll up" processing
      *                             is handled by IBM.
      *
      *  06/04/93 C:11 PROJECT D001
      *                RELEASE RECORDS TO PREVENT RECORD LOCK ON SDCMP
      *                AND SMARP.
      *
      *  09/21/95 C:30 Project:  DR011
      *           Modify "Additional Remarks" screen to allow additon of
      *           additional remarks at anytime.  Change of an existing
      *           major or change of additional remarks will be based on
      *           the value in the System Control file vs. initial entry
      *           date of the remark.
      *
      *  09/22/95 C:30 Project: DR014
      *           Modify the SCR01 display to include the Remark
      *           SEQ#.
      *
      *  03/18/96 C:45 Project: DR017
      *           ADDED USER,DATE AND TIME TO THE HEADER, AND IN
      *           THE DETAIL BASED UPON A TOGGLE F22 KEY.
      *
      *****************************************************************
      * INDICATOR USAGE
     H*  03       Office NOT installed: Nondisplay of F1=OV/400
     H* N03       Office IS installed:  Display/Use of F1 = OV/400
     H*  18    Roll up
     H*  20       Major Comment is not editable
     H*  21       Additional Remark is not editable
     H*  22       SCM0503 - Value cannot be equal to blanks.
     H*  23       Position Cursor on first available Additional Remark
     H*  30    Used to allow End of file on roll message to display
     H*        at the correct time.
     H*  32    End of file on roll up or roll down.
     H*  40       Used to chain to SDCMP
     H*  55       TOGGLE INFO TO DISPLAY USER, DATE, TIME
     H*  60       General CHAIN
     H*  61       CHAIN to additional remarks file
     H*  99       Image Installed
      *****************************************************************
      * FILE SPECIFICATIONS  * * *
      *****************************************************************
     F*** BEGIN CHG *** 10/12/15 **************************************
     F*****SC014F    CF   E             WORKSTN
      *****/If Defined (WEB)
     F*****                                Handler('PROFOUNDUI(HANDLER)')
      ****/Endif
     FSC014F    CF   E             WORKSTN Handler('PROFOUNDUI(HANDLER)')
     F*** END   CHG *** 10/12/15 **************************************
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     F*
     F*** BEGIN CHG *** 04/25/00 **************************************
     F*****SMARL   UF  E           K        DISK                      A
     FSMARL     UF   E           K DISK
     F*** END CHG *** 04/25/00 ****************************************
     FSDCMLA    UF   E           K DISK
     FSSYLF     IF   E           K DISK
     F*
      *****************************************************************
      * EXCEPTION SPECIFICATIONS * * *
      *****************************************************************

     D*
     D*
      *****************************************************************
      * INPUT SPECIFICATIONS  * * *
      *****************************************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXCM75                 1     75
     D  XXCM46                 1     46
     D  XXBLK1                47     47
     D  XXUSR                 48     57
     D  XXBLK2                58     58
     D  XXMO                  59     60  0
     D  XXSL1                 61     61
     D  XXDA                  62     63  0
     D  XXSL2                 64     64
     D  XXYR                  65     66  0
     D  XXBLK3                67     67
     D  XXHR                  68     69  0
     D  XXCL1                 70     70
     D  XXMN                  71     72  0
     D  XXCL2                 73     73
     D  XXSC                  74     75  0
     D                 DS
     D  XXIDT                  1      7  0
     D  XXYRS                  2      3  0
     D  XXMOS                  4      5  0
     D  XXDAS                  6      7  0
     D                 DS
     D  XXTRTM                 1      6  0
     D  XXHRS                  1      2  0
     D  XXMNS                  3      4  0
     D  XXSCS                  5      6  0
     D*
     D*
      * * *  FILE INFORMATION DATA STRUCTURE
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************

     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C     *LIKE         DEFINE    ARKEY         QQKEY                          12/31/91 ADD
     C     *LIKE         DEFINE    ARSEQ#        QQSEQ#                         12/31/91 ADD
     C     *LIKE         DEFINE    ARPNAM        QQPNAM                         12/31/91 ADD
     C     *LIKE         DEFINE    DRAL02        OFCPGM                         ADD 04/06/92
     C     *LIKE         DEFINE    Q2RECF        HLRECF                         ADD 04/06/92
     C     *LIKE         DEFINE    RRN2          SRRN2                          ADD 11/20/92
     C     *LIKE         DEFINE    ARASQ#        ZZASQ#                         ADD 11/20/92
     C*** BEG ADD *** 07/10/98 **************************************
     C     *LIKE         DEFINE    #@$1          HSCRR#                         ADD 07/10/98
     C*** BEG ADD *** 07/10/98 **************************************
     C*
     C*   KEY LIST FOR THE COMMENT(REMARK) DETAIL FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    QQSEQ#
     C*
     C*   KEY LIST FOR THE ADDITIONAL REMARK FILE
     C*
     C     ARKY          KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    QQSEQ#
     C                   KFLD                    XXASQ#
     C*
     C*
     C     ARKY2         KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    QQSEQ#
     C*
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY
     C                   PARM                    QQSEQ#
     C                   PARM                    QQPNAM                         12/31/91 ADD
     C*** Begin add *** 05/27/04 **************************************
     C                   PARM                    QQMODE            1
     C*** End add ***** 05/27/04 **************************************
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
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVEL     'SC'          XXSYK
     C                   MOVE      *OFF          *IN55
     C                   MOVE      *BLANKS       XXBLK1
     C                   MOVE      *BLANKS       XXBLK2
     C                   MOVE      *BLANKS       XXBLK3
     C                   MOVE      '/'           XXSL1
     C                   MOVE      '/'           XXSL2
     C                   MOVE      ':'           XXCL1
     C                   MOVE      ':'           XXCL2
     C*
     C*** Check System Control File Install Flags for this Client:
     C     'SY'          CHAIN     SSY01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   Check for AS/400 Office Installed
     C     SYOFIF        IFEQ      'Y'
     C                   MOVE      '0'           *IN03                          Installed
     C                   ELSE                                                   ELS 'Y' SYOFIF
     C                   MOVE      '1'           *IN03                          Not Installed
     C                   END                                                    END 'Y' SYOFIF
     C*
     C                   END                                                    END 0 *IN60
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
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C/EJECT
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C     *IN11         IFEQ      '1'
     C*
     C****************************************
     C*   F1 = OV/400                        *
     C****************************************
     C     *INKA         IFEQ      '1'
     C                   MOVE      *BLANKS       QQRTCD            1
     C                   MOVE      *BLANKS       OFCPGM
     C                   MOVEL     Q2RECF        HLRECF
     C*
     C* Get Office CLP Program Name from SDRPF/DRAL02
     C     SYKY          CHAIN     SDR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     DRAL02        OFCPGM
     C                   END
     C*
     C                   CALL      OFCPGM
     C                   PARM                    QQRTCD
     C*
     C                   WRITE     SCR99
     C                   MOVEL     HLRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END                                                    END *INKA
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C*
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C*
     C                   END                                                    END *INKL
     C*
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN30         IFEQ      '1'
     C                   MOVE      '1'           *IN32
     C                   MOVE      '0'           *IN30
     C                   END
     C*
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C*
     C     ARKY          SETGT     PAR01
     C*
     C                   EXSR      LOAD
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C****************************************
     C*   F22 = TOGGLE VIEW                  *
     C****************************************
     C     *INKW         IFEQ      *ON
     C*
     C     *IN55         IFEQ      *OFF
     C                   MOVE      *ON           *IN55
     C                   ELSE
     C                   MOVE      *OFF          *IN55
     C                   ENDIF
     C*
     C                   Z-ADD     *ZEROS        XXASQ#
     C     ARKY          SETGT     PAR01
     C*
     C                   EXSR      LOAD
     C*
     C                   ENDIF
     C*
     C****************************************
     C*   *IN10 -  PROCESS HELP REQUESTS     *
     C****************************************
     C  N60
     CAN 60              MOVE      '0'           *IN10                            TEMP * STUFF
     C     *IN10         IFEQ      '1'
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C*
      *****************************************************************
      *   GOTO THE APPROPRIATE SCREEN
      *****************************************************************
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR01S'      SCR01T
     C     Q2RECF        CABEQ     'SCR01C'      SCR01T
     C*
     C*   PROCESS REQUESTED SCREEN
     C*
     C*
      *------------------------*
     C     SCR01T        TAG
      *------------------------*
     C*
     C                   MOVE      ' '           ERROR             1
     C                   MOVEA     '000'         *IN(17)
     C*
     C                   READC     SCR01S                                 60
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     RRN2          XXASQ#            5 0
     C     ARKY          CHAIN     PAR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SCCM75        ARCM75
     C                   MOVEL     CMSECL        ARSECL
     C                   MOVEL     CMSBYL        ARSBYL
     C                   MOVEL     CMSTAT        ARSTAT
     C                   MOVEL     Q1USER        ARUSER
     C                   MOVEL     Q1PGM         ARPGM
     C                   MOVE      DSTIME        ARTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   UPDATE    PAR01
     C                   ELSE
     C*** BEGIN DEL *** 04/25/00 **************************************
     C********             MOVELQQKEY     ARKEY
     C********             MOVE QQSEQ#    ARSEQ#
     C********             MOVELSCCM75    ARCM75
     C********             MOVE RRN2      ARASQ#
     C********             MOVELCMCSID    ARCSID
     C********             MOVE CMCDAT    ARCDAT
     C********             MOVELCMRMCD    ARRMCD
     C********             MOVELQQPNAM    ARPNAM           12/31/91 CHG
     C********             TIME           DSDTTM
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARTRDT
     C********
     C********             MOVE DSTIME    ARTRTM
     C********             MOVELCMSECL    ARSECL
     C********             MOVELCMSBYL    ARSBYL
     C********             MOVELCMSTAT    ARSTAT
     C********             MOVELQ1USER    ARUSER
     C********             MOVELQ1PGM     ARPGM
     C********             MOVE DSTIME    ARTIME
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARDT
     C********
     C********             MOVE ARDT      ARIDT
     C********             MOVELQ1USER    ARIUSR
     C********
     C********             WRITEPAR01
     C*** END DEL *** 04/25/00 ****************************************
     C*** BEGIN ADD *** 04/25/00 **************************************
     C*
     C*          Call API to write a DARTS Additional Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QQKEY         Q#KEY            40
     C                   MOVE      QQSEQ#        Q#SEQ#            5
     C                   MOVEL(P)  SCCM75        Q#CM75           75
     C                   MOVEL(P)  QQPNAM        Q#PNAM           10
     C                   MOVE      CMSECL        Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  CMRMCD        Q#RMCD            5
     C                   MOVE      '00000'       Q#ASQ#            5
     C                   MOVEL(P)  CMCSID        Q#CSID            2
     C                   MOVE(P)   CMCDAT        Q#CDAT            7
     C                   CALL      'DA701R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#CM75
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#ASQ#
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CDAT
     C*** END ADD *** 04/25/00 ****************************************
     C*
     C                   END
     C*
     C                   UPDATE    SCR01S
     C*
     C*
     C     NXTR          TAG
     C*
     C                   READC     SCR01S                                 60
     C                   END
     C*
     C*  Restore RRN2 so that rolling can properly function
     C*
     C                   MOVE      SRRN2         RRN2
     C*
     C*  MAJOR COMMENT CANNOT BE EQUAL TO BLANKS
     C*
     C     SCCOMM        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  IF MAJOR COMMENT HAS CHANGED, SAVE NEW VALUE IN SDCMP
     C*
     C     CMKY          CHAIN     PCM01                              40
     C     *IN40         IFEQ      '0'
     C     CMCOMM        IFNE      SCCOMM
     C                   MOVEL     SCCOMM        CMCOMM
     C                   UPDATE    PCM01
     C                   ELSE
     C                   EXCEPT    RCM01
     C                   END
     C                   END
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
     C/EJECT
     C*
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C*
     C     INIT          BEGSR
     C                   TIME                    DSDTTM
     C                   MOVE      '0'           XXOPN             1
     C                   MOVE      *HIVAL        @@TIME            6 0
     C                   Z-ADD     0             RRN2              5 0
     C                   Z-ADD     0             HLDRRN            5 0
     C                   Z-ADD     0             HLDRR2            5 0
     C                   Z-ADD     0             XXTIME            6 0
     C                   Z-ADD     0             XXDATE            6 0
     C                   Z-ADD     0             XXSEQ#            5 0
     C                   MOVE      *HIVAL        X#SEQ#            5 0
     C                   Z-ADD     0             XXRRN1            5 0          ADD 11/20/92
     C                   MOVE      *ON           ADDFLG            1
     C*
     C*** Begin add *** 05/27/04 **************************************
     C                   SETOFF                                           09
     C     Q1PRMS        IFGT      3
     C     QQMODE        COMP      'D'                                    09
     C                   ENDIF
     C*** End add ***** 05/27/04 **************************************
     C*
     C     SYKY          CHAIN     SDR01                              60
     C     *IN60         IFEQ      '0'
     C     DRFL01        ANDEQ     'Y'
     C                   MOVE      '1'           *IN99                            IMAGE INSTALLED
     C                   END
     C*
     C     CMKY          CHAIN     PCM01                              60        ADD 11/20/92
     C     *IN60         IFEQ      '0'                                          ADD 11/20/92
     C*
     C                   EXCEPT    RCM01
     C*
     C*  The following code was moved here from the LOAD subroutine
     C*
     C*  CALCULATE # OF DAYS SINCE INITIAL DATE OF MAJOR REMARK
     C*
     C                   Z-ADD     CMIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFROM            8 0
     C*
     C                   MOVE      XXFROM        QQFROM            8 0
     C*
     C***                  Z-ADDDSDATE    SRDTI
     C***                  CALL 'SRCVTDAT'DTMDY8
     C***                  MOVELSRDTO     QQTO    80
     C                   Z-ADD     DSDATE        QQTO              8 0
     C*
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*
     C*  IF # DAYS ON SYSTEM IS > # DAYS AFTER EACH RECORD'S INITIAL
     C*     ENTRY DATE, RECORD IS UPDATEABLE.            (*IN20 = 0)
     C*  ELSE IF # DAYS ON SYSTEM IS <= # DAYS AFTER EACH RECORD'S
     C*     INITIAL ENTRY DATE RECORD IS PROTECTED.      (*IN20 = 1)
     C*
     C     DRDT01        IFLE      QQDAYS
     C                   MOVE      '1'           *IN20
     C                   ELSE
     C                   MOVE      '0'           *IN20
     C                   END
     C*
     C                   END                                                    ADD 11/20/92
     C*
     C*** INITIALIZE SUBFILE
     C                   MOVE      '1'           *IN02
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN02
     C*
     C                   Z-ADD     0             XXASQ#
     C*
     C     ARKY          SETLL     PAR01
     C                   EXSR      LOAD
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   ENDSR
     C*
     C*
      *====================================================================*
      * * *   LOAD SUBROUTINE                                    I N I T ***
      *====================================================================*
     C*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   MOVE      '0'           *IN69
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   EXCEPT    RCM01
     C*
     C                   MOVEL     CMDCID        SCDCID
     C*
     C                   Z-ADD     CMCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCDAT
     C*
     C                   MOVEL     CMRMCD        SCCODE
     C                   MOVEL     CMCOMM        SCCOMM
     C*
     C***  BEGIN CHG  ***  07/23/98  *********************************
     C********** CMIMGF    IFEQ 'Y'
     C**********           MOVEL'Image :' SCDOCH
     C**********           ELSE
     C**********           MOVEL'Doc ID:' SCDOCH
     C**********           END
     C     CMIMGF        IFEQ      'Y'
     C                   MOVEL     'Image:'      SCDOCH
     C                   ELSE
     C                   MOVEL     'DocID:'      SCDOCH
     C                   END
     C***  END   CHG  ***  07/23/98  *********************************
     C*
     C                   Z-ADD     CMSEQ#        SCSQ#
     C                   MOVEL     CMIUSR        SCUSER
     C*
     C                   Z-ADD     CMIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDATE
     C*
     C                   Z-ADD     CMTRTM        SCTIME
     C*
     C*  The code that was previously here was moved to the
     C*           INIT subroutine.
     C*
     C*
     C                   END
     C*
     C     ARKY2         READE     PAR01                                  60
     C  N60              EXCEPT    RAR01
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *OFF          ADDFLG
     C                   Z-ADD     1             #@$1
     C                   ENDIF
     C*
     C     *IN60         DOWEQ     '0'
     C     ARASQ#        CHAIN     SCR01S                             61
     C     *IN61         IFEQ      '0'
     C*
     C*  The following code was deleted per project dated 11/20/92.
     C*  It is being re-instated per project dated 09/21/95.
     C*
     C*  CALCULATE # OF DAYS SINCE INITIAL DATE OF ADDITIONAL REMARKS
     C*
     C                   Z-ADD     ARIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFROM
     C*
     C                   MOVE      XXFROM        QQFROM
     C                   Z-ADD     *ZERO         QQDAYS
     C                   MOVE      *BLANK        QQERR
     C                   MOVE      '1'           QQTERM
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*
     C*  IF # DAYS ON SYSTEM IS > # DAYS AFTER EACH RECORD'S INITIAL
     C*     ENTRY DATE, RECORD IS UPDATEABLE. (*IN21 = 0)
     C*  ELSE IF # DAYS ON SYSTEM IS <= # DAYS AFTER EACH RECORD'S
     C*     INITIAL ENTRY DATE RECORD IS PROTECTED.      (*IN21 = 1)
     C*
     C     DRDT01        IFLE      QQDAYS
     C                   MOVE      '1'           *IN21
     C                   ELSE
     C                   MOVE      '0'           *IN21
     C     ADDFLG        IFEQ      *ON
     C                   MOVE      *ON           *IN23
     C                   MOVE      *OFF          ADDFLG
     C                   Z-ADD     ARASQ#        #@$1
     C                   ENDIF
     C                   END
     C*** BEG ADD *** 07/10/98 **************************************
     C     XX1ST         IFEQ      ' '
     C                   Z-ADD     ARASQ#        HSCRR#
     C                   MOVE      *ON           XX1ST             1
     C                   ENDIF
     C*** BEG ADD *** 07/10/98 **************************************
     C*
     C     *IN55         IFEQ      *OFF
     C                   MOVEL     ARCM75        SCCM75
     C                   ELSE
     C                   Z-ADD     ARIDT         XXIDT
     C                   Z-ADD     ARTRTM        XXTRTM
     C                   Z-ADD     XXYRS         XXYR
     C                   Z-ADD     XXMOS         XXMO
     C                   Z-ADD     XXDAS         XXDA
     C                   Z-ADD     XXHRS         XXHR
     C                   Z-ADD     XXMNS         XXMN
     C                   Z-ADD     XXSCS         XXSC
     C                   MOVEL     ARIUSR        XXUSR
     C                   MOVEL     ARCM75        XXCM46
     C                   MOVEL     XXCM75        SCCM75
     C                   ENDIF
     C                   UPDATE    SCR01S
     C                   END
     C     ARKY2         READE     PAR01                                  60
     C  N60              EXCEPT    RAR01
     C*
     C                   END
     C*
     C     ADDFLG        IFEQ      *ON
     C                   MOVE      *OFF          ADDFLG
     C     ARASQ#        ADD       1             X                 3 0
     C                   Z-ADD     X             #@$1
     C     X             CHAIN     SCR01S                             61
     C     *IN61         IFEQ      '0'
     C                   MOVE      *OFF          *IN21
     C                   MOVE      *ON           *IN23
     C                   UPDATE    SCR01S
     C                   ENDIF
     C                   ENDIF
     C*
     C*** BEG ADD *** 07/10/98 **************************************
     C***                  ELSE
     C     XX1ST         IFEQ      *ON
     C                   Z-ADD     HSCRR#        #@$1
     C                   ENDIF
     C*** END ADD *** 07/10/98 **************************************
     C                   MOVE      ' '           XX1ST                          ADD 07/10/98
     C                   ENDSR
     C*
     OPCM01     E            RCM01
     OPAR01     E            RAR01
