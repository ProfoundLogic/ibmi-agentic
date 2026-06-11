     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE SC012R - DISPLAY REMARK DETAIL
     H******************************************************************
      * REVISIONS:
      *
      *  10/17/11 C:277 Project: SY195
      *                 QDLS migration to IFS
      *                 Add two parameters with command
      *
      * 05/07/07 C:180 Support: WA133 56379
      *                Update maintenance pgm for policy page record.
      *
      * 12/12/01 C:128 Project: VI114 #30191                          *
      *                Added Scan Batch ID to screen. Allows easier   *
      *                identification of document.                    *
      *
      * 12/15/99  C:15  Project:
      *                 Changed pgm to be retrieve more accurate data
      *                 for maint fields.
      *
      * 10/05/98  C:15  Support: DR031/14177
      *                 Added a parm to the call to SADDDOC to prevent
      *                 an error message for occurring do to parameter
      *                 mismatch.
      *
      * 07/31/98  C:98  PROJECT:  DR062
      *                 MODIFY THE DISPLAY OF THE REMARK DETAIL TO
      *                 INDICATE THE "LAST MAINTENACE" INFORMATION
      *                 BASED ON THE MAJOR REMARK OR ADDITIONAL
      *                 REMARKS, AS APPROPIATE
      *
      * 12/18/91 LJD    - Proj: SC083
      *                   1.  Added variable XXOFIF to control AS/400 Office
      *                       processing.  If XXOFIF = 'N', and user chose
      *                       an Office Function, indicator 31 turned on.
      *                       Added to prevent "bombs" on Office processing
      *
      * 02/08/92 LJD    - Proj: SC090
      *                   1.  Pass PNAM as a parm into SC014R to update ARPNAM
      *                       with keyed program name
      *
      * 03/23/92 WLD    - Proj: AJP
      *                   1.  Changed 2A Remark Code to 5A. (Changed    ARPNAM
      *                       reference variable for SCCODE in screen   ARPNAM
      *                       file to REFFLD(CMRMCD *LIBL/SDCMP)
      *
      *  04/06/92 LJD   - Proj: ANI
      *                   1.  Added F1 = OV/400 based upon program name
      *                       in DARTS System Control File SDRPF/DRAL02
      *                   2.  Added F24 = More Keys processing because
      *                       there are too many command keys for 1 line
      *  07/12/98 C:02  - BASE MOD
      *                   1. ALLOW CHANGE OF OVE DOC AND FOLDER IF
      *                      CTFL03 EQ 'Y'
      *
      *  ADD NEW REVISION REMARKS AT TOP OF REVISION SECTION
      *
     H******************************************************************
     H* INDICATOR USAGE                                                *
     H*  03       Office NOT installed: Nondisplay of F1=OV/400
     H* N03       Office IS installed:  Display/Use of F1 = OV/400
     H*  20       ERROR -  FOLDER MUST BE ENTERED IF DOC ID ENTERED.   *
     H*  21       SCM0521-FOLDER DOES NOT EXIST                        *
     H*  22       SCM0517-NOT AUTHORIZED TO FOLDER                     *
     H*  23       ERROR -  INVALID DOCUMENT                            *
     H*  24       SCM0523-NOT AUTHORIZED TO ADD OFFICE DOCUMENT        *
     H*  25       SCM0531-UNABLE TO PROCESS THIS FOLDER                *
     H*  30       ALLOW ADD OF DOCUMENT NAME AND FOLDER TO REMARK      *
      *  31      SCM0550-AS/400 Officevision is not installed
     H*  40       CHAIN TO SDCMLA                                      *
     H*  91       SCM0525-DOC NAME MUST BE VALID TO PERFORM ADD FUNCTN *
     H*  92       SCM0517-NOT AUTHORIZED TO DOCUMENT OR FOLDER         *
     H*  93       SCM0518-USER NOT ENROLLED IN OFFICE 400              *
     H*  94       SCM0519-UNABLE TO PROCESS REQUEST FOR THIS DOCUMENT  *
     H*  95       SCM0520-DOCUMENT NAME ALREADY EXISTS AS DOC OR FLDR  *
     H*  96       SCM0521-FOLDER NOT FOUND                             *
     H******************************************************************
     FSC012F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSDCMLA    UF   E           K DISK
     FSMSLL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     FSMARL     IF   E           K DISK
      *** BEGIN ADD *** 07/12/98 **************************************
     FSTCTL1    IF   E           K DISK
      *** END ADD *** 07/12/98 ****************************************
      *** BEGIN ADD *** 12/12/01 **************************************
     FIMDOL     IF   E           K DISK    USROPN
      *** END ADD *** 12/12/01 ****************************************
      *

     D CK1             S             70    DIM(4) CTDATA PERRCD(1)              COMMAND KEYS - SCR01
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    QQSEQ#            5 0
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
     C*   KEY LIST FOR THE COMMENT FILE (REMARKS)
     C*
     C     CMKY1         KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    QQSEQ#
     C*
     C*   KEY LIST FOR THE SECURITY FILE
     C     SLKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*
     C     ARKY          KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    QQSEQ#
      *** BEGIN ADD *** 07/12/98 **************************************
      *
      * KEY LIST FOR THE REMARK CODE TYPE FILE
      *
     C     CTKY          KLIST
     C                   KFLD                    SCCODE
      *** END ADD *** 07/12/98 ****************************************
     C*
     C* INITIALIZATION SECTION
     C     *LIKE         DEFINE    CMPNAM        #7PNAM                         ADD 02/08/92
     C     *LIKE         DEFINE    DRAL02        OFCPGM                         ADD 04/06/92
     C     *LIKE         DEFINE    Q2RECF        HLRECF                         ADD 04/06/92
     C                   Z-ADD     0             C1                5 0          ADD 04/06/92
     C*
     C                   TIME                    DSDTTM
     C*
     C*
     C                   MOVEL     'SC'          XXSYK
     C*
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     CMKY1         CHAIN     PCM01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PCM01E
     C*
     C*
     C     SYKY          CHAIN     SDR01                              60
     C     *IN60         IFEQ      '0'
     C     DRFL01        ANDEQ     'Y'
     C                   MOVE      '1'           *IN99
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
     C                   MOVE      *BLANKS       OFCPGM
     C*
     C*  If Officevision/400 is not installed, use first CMD line
     C     *IN03         IFEQ      '1'
     C                   Z-ADD     1             C1
     C                   MOVEL     CK1(C1)       SCCML1
     C                   ELSE
     C*  If Officevision/400 is installed, use second CMD line
     C                   Z-ADD     2             C1
     C                   MOVEL     CK1(C1)       SCCML1
     C*  Establish Officevision program to be called if F1 pressed
     C                   MOVEL     DRAL02        OFCPGM
     C                   END
     C*
     C*** BUILD KEY DISPLAY HERE
     C*
     C                   MOVEL     CMKEY         X#KEY
     C                   MOVE      ' '           X#RTCD
     C                   MOVEL     CMPNAM        X#PNAM
     C                   CALL      'SRKCVT'
     C                   PARM                    X#PNAM           10
     C                   PARM                    X#KEY            79
     C                   PARM                    X#RTCD            1
     C*
     C     X#RTCD        IFNE      'N'
     C                   MOVEL     X#KEY         SCKEY
     C                   ELSE
     C                   MOVEL     CMKEY         SCKEY
     C                   END
     C*
     C                   Z-ADD     CMCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCDAT
     C*
     C                   MOVEL     CMRMCD        SCCODE
     C                   MOVEL     CMDCID        SCDCID
     C                   MOVEL     CMCOMM        SCCOMM
     C*
     C                   Z-ADD     CMTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTRDT
     C*
     C                   Z-ADD     CMTRTM        SCTRTM
     C*** BEGIN DLT *** 12/15/99 **************************************
     C******************** MOVELCMPNAM    SCPNAM
     C*** END DLT ***** 12/15/99 **************************************
     C*** BEGIN ADD *** 12/15/99 **************************************
     C                   MOVEL     CMIPGM        SCIPGM
     C*** END ADD ***** 12/15/99 **************************************
     C                   MOVE      CMSECL        SCSECL
     C                   MOVE      CMSBYL        SCSBYL
     C                   MOVEL     CMSTAT        SCSTAT
     C                   MOVEL     CMUSER        SCUSER
     C                   MOVEL     CMPGM         SCPGM
     C*** IF ADDITIONAL COMMENTS EXIST, THEN SPECIFY "MORE..." FOR COMMENT.
     C*
     C     ARKY          CHAIN     PAR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     'More...'    XXMORE            9
     C                   MOVE      ''           XXMORE
     C                   MOVE      XXMORE        SCCOMM
     C                   END
     C*
     C* * * IMAGE FIELDS
     C                   MOVEL     CMIQUE        SCIQUE                             HIDDEN
     C                   MOVEL     CMIPTH        SCIPTH                             HIDDEN
     C                   MOVE      CMPIDX        SCPIDX                             HIDDEN
     C                   MOVEL     CMAPID        SCAPID                             HIDDEN
     C                   MOVEL     CMIDOC        SCIDOC                             HIDDEN
     C                   MOVEL     CMQPTY        SCQPTY                             HIDDEN
     C                   MOVEL     CMDCTP        SCDCTP                             HIDDEN
      *** BEGIN ADD *** 12/12/01 **************************************
     C     *IN99         IFEQ      *ON
     C                   OPEN      IMDOL
     C     CMKY1         CHAIN     PDO01                              59
     C                   MOVEL     DOBCHI        SCBCHI
     C                   ENDIF
      *** END   ADD *** 12/12/01 **************************************
     C*
     C                   Z-ADD     CMTIME        SCTIME
     C*
     C                   Z-ADD     CMDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT
     C*
     C                   MOVEL     CMIUSR        SCIUSR
     C*
     C                   MOVEL     CMFOLD        SCFOLD
     C*
     C                   Z-ADD     CMDCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDCDT
     C*
     C                   MOVEL     CMDIFL        SCDIFL
     C*
     C                   MOVEL     *BLANK        SCSDES
     C     SCSTAT        IFEQ      'I'
     C                   MOVEL     'DELETED '    SCSDES
     C                   ELSE
     C                   MOVEL     'ACTIVE'      SCSDES
     C                   END
     C*
     C     SCDCID        IFEQ      *BLANK
     C                   MOVE      '1'           *IN30
     C                   END
      *** BEGIN ADD *** 07/12/98 **************************************
      * IF THE FLAG FOR THIS REMARK TYPE IS 'Y' ALLOW EDITING OF THE
      * DOCUMENT/FOLDER ID
      *
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C     CTFL03        ANDEQ     'Y'
     C                   MOVE      *ON           *IN32
     C                   ENDIF
      *** END ADD *** 07/12/98 ****************************************
     C*
     C                   ELSE
     C                   GOTO      TERM
     C*
     C                   END
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C**** BEGIN ADD **** 07/31/98 ************************************
     C*** IF ADDITIONAL COMMENTS EXIST, THEN USE SMARP RECORD
     C*                                 ELSE USE SDCMP RECORD
     C*
     C*** BEGIN DLT *** 12/15/99 **************************************
     C********** ARKY      CHAINPAR01                60
     C*** END DLT ***** 12/15/99 **************************************
     C*** BEGIN ADD *** 12/15/99 **************************************
     C     ARKY          SETGT     PAR01
     C     ARKY          READPE    PAR01                                  60
     C*** END ADD ***** 12/15/99 **************************************
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVEL     ARPGM         SCPGM
     C*** BEGIN CHG *** 12/15/99 **************************************
     C******************** MOVELARIUSR    SCMUSR
     C                   MOVEL     ARUSER        SCMUSR
     C*** END CHG ***** 12/15/99 **************************************
     C                   Z-ADD     ARTIME        SCTIME
     C*
     C                   Z-ADD     ARIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT
     C*
     C                   ELSE
     C*
     C                   MOVEL     CMPGM         SCPGM
     C                   MOVEL     CMIUSR        SCMUSR
     C                   Z-ADD     CMTIME        SCTIME
     C*
     C                   Z-ADD     CMIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT
     C*
     C                   ENDIF                                                  IN60 EQ 0
     C*
     C**** END   ADD **** 07/31/98 ************************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C**********************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C****************************************
     C*   F1 = OV/400                        *
     C****************************************
     C     *INKA         IFEQ      '1'
     C                   MOVEL     Q2RECF        HLRECF
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      OFCPGM
     C                   PARM                    QQRTCD
     C                   WRITE     SCR99
     C                   MOVEL     HLRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *INKA
     C********************************************
     C*   F3 = EXIT                              *
     C********************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*   F5 = ADD OFFICE DOCUMENT           *
     C****************************************
     C     *INKE         IFEQ      '1'
     C*
     C*** If AS/400 Office NOT Installed - Don't allow this function
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      '0'           *IN24
     C                   MOVEA     '000000'      *IN(91)
     C*
     C*** MUST SEE IF USER IS ALLOWED TO ADD DOCUMENT
     C     SLKEY         CHAIN     PSL01                              60
     C     *IN60         IFEQ      '1'
     C     SLADC         ORNE      'Y'
     C                   MOVE      '1'           *IN24                          ADD NOT ALLOWED
     C                   GOTO      STRT
     C                   END
     C*
     C     SCDCID        CABEQ     *BLANK        STRT                     91
     C*
     C                   MOVEL     SCDCID        XXDCID
     C                   MOVEL     SCFOLD        XXFOLD           80            ADD 1/21/93  02
     C                   MOVEL     *BLANK        QQRTCD            1
     C                   CALL      'SADDDOC'
     C                   PARM                    XXDCID           12
     C                   PARM                    XXFOLD
     C                   PARM                    QQRTCD
     C*** BEGIN ADD *** 10/05/98 ***********************************
     C                   PARM      ' '           QQMS              1
     C*** END ADD ***** 10/05/98 ***********************************
     C*** Begin Add *** 10/17/11 **************************************
     C                   PARM      *BLANKS       XXUNCN          255
     C                   PARM      *BLANKS       XXFSF             1
     C*** End   Add *** 10/17/11 **************************************
     C*
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN92
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      '1'           *IN93
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      '1'           *IN94
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '5'
     C                   MOVE      '1'           *IN95
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '6'
     C                   MOVE      '1'           *IN96
     C                   GOTO      STRT
     C                   END
     C*
     C                   GOTO      STRT
     C                   END                                                    END *INKE
     C****************************************
     C*  F6 = SEARCH DOCUMENT IN A FOLDER.  *
     C****************************************
     C     *INKF         IFEQ      '1'
     C*
     C*** If AS/400 Office NOT Installed - Don't allow this function
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      '0'           *IN25
     C                   MOVEA     '000000'      *IN(91)
     C*
     C                   MOVEL     *BLANK        XXDCID                         RETURN DOC ID
     C*
     C*** IF SCREEN FOLDER IS BLANK, USE USER'S DEFAULT FOLDER
     C     SCFOLD        IFEQ      *BLANKS
     C                   MOVEL     SLFOLD        XXFOLD           80            DEFAULT FOLDER
     C                   ELSE
     C                   MOVEL     SCFOLD        XXFOLD           80            SCREEN FOLDER
     C                   END
     C*
     C                   MOVEL     *BLANK        QQRTCD            1            ERR RTN CODE
     C*
     C                   CALL      'SDSPFLR'                                    "DSPFLR" OFC API
     C                   PARM                    XXFOLD                         DFLT FOLDER
     C                   PARM                    XXDCID           12            RTN DOC ID SLCT
     C                   PARM                    QQRTCD                         ERR ID CODE
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN91
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN92
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      '1'           *IN93
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '5'
     C                   MOVE      '1'           *IN95
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '6'
     C                   MOVE      '1'           *IN96
     C                   GOTO      STRT
     C                   END
     C*
     C     XXDCID        IFNE      *BLANK                                       VALID DOC ID
     C                   MOVEL     XXDCID        SCDCID                         USE IT, GOTO
     C                   GOTO      STRT                                         START
     C                   END
     C*
     C                   GOTO      STRT
     C                   END                                                    END *INKF
     C****************************************
     C*  F7 = ADDITIONAL REMARKS CALL       *
     C****************************************
     C     *INKG         IFEQ      '1'
     C*
     C                   MOVEL     QQKEY         #7KEY            40
     C                   MOVE      QQSEQ#        #7SEQ#            5 0
     C                   MOVEL     CMPNAM        #7PNAM                         ADD 02/08/92
     C                   CALL      'SC014R'
     C                   PARM                    #7KEY
     C                   PARM                    #7SEQ#
     C                   PARM                    #7PNAM                         ADD 02/08/92
     C*
     C*
     C                   GOTO      STRT
     C                   END                                                    END *INKF
     C********************************************
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKL
     C********************************************
     C*   *IN10 = PROCESS HELP REQUESTS          *
     C********************************************
     C  N10
     CAN 10*IN10         IFEQ      '1'
     C  N10
     CAN 10              MOVE      '1'           *IN10
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
     C*
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C****************************************
     C*   F24=More Keys                      *
     C****************************************
     C     *INKY         IFEQ      '1'
     C*
     C*  If Officevision/400 is not installed,
     C*      add to C1 to get inline for next CMD line (#3)
     C     *IN03         IFEQ      '1'
     C     C1            ANDEQ     1
     C                   ADD       1             C1
     C                   END                                                    END 1 *IN03
     C*
     C     C1            ADD       1             C1
     C*
     C     C1            IFGT      4
     C     *IN03         IFEQ      '1'
     C                   Z-ADD     1             C1
     C                   ELSE                                                   ELS 1 *IN03
     C                   Z-ADD     2             C1
     C                   END                                                    END 1 *IN03
     C                   END                                                    END GT 3
     C*
     C                   MOVEL     CK1(C1)       SCCML1
     C*
     C                   GOTO      STRT
     C                   END                                                    END *INKY
     C********************************************
     C                   END                                                    END CMDKEYS
     C*************************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C     *IN30         IFEQ      '1'                                          ALLOW DOC ID/
     C*                                                     FOLDER TO BE
     C** IF DOCID THEN FOLDER MUST BE ENTERED.
     C*
     C     SCDCID        IFNE      *BLANK
     C*
     C*** If AS/400 Office NOT Installed - Don't allow this function
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C     SCFOLD        CABEQ     *BLANK        STRT                     20
     C                   END
     C*                                                     ENTERED.
     C*** EDIT FOR VALID FOLDER.(IF NOT BLANK)
     C*
     C     SCFOLD        IFNE      *BLANK
     C*
     C*** If AS/400 Office NOT Installed - Don't allow this function
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANK        QQRTCD
     C                   CALL      'SVALFLR'
     C                   PARM                    SCFOLD
     C                   PARM                    QQRTCD
     C*
     C     QQRTCD        CABEQ     '1'           STRT                     21     INVALID FOLDER NAME
     C     QQRTCD        CABEQ     '2'           STRT                     22     NOT AUTHORIZED.
     C                   END
     C*
     C*** EDIT FOR VALID DOCID. (IF NOT BLANK)
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
     C                   MOVEL     SCDCID        XXDCID           12
     C                   MOVEL     SCFOLD        XXFOLD           80
     C                   MOVEL     *BLANK        QQRTCD            1
     C*
     C                   CALL      'SVALDOC'
     C                   PARM                    XXDCID           12
     C                   PARM                    XXFOLD
     C                   PARM                    QQRTCD
     C*
     C     QQRTCD        CABEQ     '1'           STRT                     23
     C*
     C                   END                                                    DCID <> *BLANK
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD
     C*
     C     *IN30         IFEQ      '1'
      *** BEGIN ADD *** 07/12/98 **************************************
     C     *IN32         OREQ      *ON
      *** END ADD ** 07/12/98 *****************************************
     C     SCDCID        ANDNE     *BLANK
     C     CMKY1         CHAIN     PCM01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     SCFOLD        CMFOLD
     C                   MOVEL     SCDCID        CMDCID
      *********** BEGIN ADD  *************** 05/07/07 *****************
     C     CMRMCD        IFEQ      'PSPP'
     C                   MOVE      Q1PGM         CMPGM
     C                   ENDIF
      *********** END ADD  ***************** 05/07/07 *****************
     C                   UPDATE    PCM01
     C                   END
     C                   END
     C*
     C                   GOTO      STRT                                         REDISPLAY
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPCM01     E            PCM01E
**  CK1 - Command Keys - SCR01
F3=Exit             F5=Add office document               F24=More keys
F1=OV/400      F3=Exit      F5=Add office document       F24=More keys
F6=View documents in a folder   F7=Additional Remarks    F24=More keys
F12=Previous                                             F24=More keys
