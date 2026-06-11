     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SC016R - DIARY ANOTHER USER
     H*  Program Description: This allows the user to pass a diary item
     H*                       to another user.
     H*  Project: ANE
     H*****************************************************************
     H** REVISIONS:
     H*  04/25/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SMARP with API DA701R
     H*
      *  11/19/99 C:113 PROJECT: DR079
      *                 Corrected security flag usage.
      *
      *  07/21/98 C:98  PROJECT: DR060
      *                   1.  SET CMFL04 IF REMARK DIARIED TO ANOTHER
      *                       USER.  SAMPLE FROM B70 SOURCE
      *
      *  07/20/98 C:98  PROJECT: DR060
      *                   1.  CHANGED CMUSER TO Q1USER FOR DIARY
      *                       ANOTHER USER COMPLETION MESSAGE.
      *
      *  06/30/92 WLD   - Proj: ANE
      *                   1.  Released chain to SDCMLA after display
      *                       and "rechained" before update in order
      *                       to allow the user to enter additional
      *                       remarks with F7.
      *
      *  09/03/92 WLD   - Proj: ???
      *                   1.  Changed User search from SC650C
      *                       (User Profile Name Search) to SC530F/R
      *                       (DARTS Security File Search).
      *
      *  PUT FUTURE REVISIONS AT TOP
      *
     H*****************************************************************
     H* INDICATOR USAGE
     H*****************************************************************
     H* FILE SPECIFICATIONS  * * *
     H*****************************************************************
     FSC016F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F*
     FSDCMLA    UF   E           K DISK
     FSMSLL     IF   E           K DISK
     F*** BEGIN CHG *** 04/25/00 **************************************
     F*****SMARL   UF  E           K        DISK                      A
     F*** BEGIN DLT *** 09/12/97 ***********************************************
     F*

     D*
     D*    COMMENT USED TO WRITE DIARY COMPLETION INFORMATION TO
     D*       ADDITIONAL REMARKS FILE
     D*
     D CMT             S             75    DIM(1) CTDATA PERRCD(1)
     D*
      *****************************************************************
      * INPUT SPECIFICATIONS  * * *
      *****************************************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D  XXSM                   7      8
     D  XXSD                   9     10
     D  XXSY                  11     12
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  CMCDAT                 1      7  0
     D  XXCY                   2      3  0
     D  XXCM                   4      5  0
     D  XXCD                   6      7  0
     D*
     D*    COMMENT STRING
     D*
     D                 DS
     D  @@CMT                  1     75
     D  @@USER                21     30
     D  @@CM                  42     43
     D  @@CD                  45     46
     D  @@CY                  48     49
     D  @@SM                  68     69
     D  @@SD                  71     72
     D  @@SY                  74     75
     D*
     D*
     D* * *  FILE INFORMATION DATA STRUCTURE
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     D* * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************

     C     *LIKE         DEFINE    CMPNAM        #7PNAM
     C*** BEGIN DEL *** 04/25/00 **************************************
     C********   *LIKE     DEFN ARASQ#    XXASQ#           ADD 01/11/93
     C********   *LIKE     DEFN ARASQ#    XXNXT#           ADD 01/11/93
     C*** END DEL *** 04/25/00 ****************************************
     C     *LIKE         DEFINE    CMUSER        XXNUSR
     C*
     C*   KEY LIST FOR THE COMMENT(REMARK) DETAIL FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    XXSEQ#
     C*
     C*   KEY LIST FOR THE ADDITIONAL REMARK FILE
     C*
     C*** BEGIN DEL *** 04/25/00 **************************************
     C********   ARKY      KLIST
     C********             KFLD           QQKEY
     C********             KFLD           XXSEQ#
     C********             KFLD           XXASQ#
     C********
     C********   ARKY2     KLIST
     C********             KFLD           QQKEY
     C********             KFLD           XXSEQ#
     C*** END DEL *** 04/25/00 ****************************************
     C*
     C*   KEY LIST FOR THE SECURITY FILE
     C*
     C     SLKY          KLIST
     C                   KFLD                    XXNUSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    QQSEQ#            5
     C                   PARM                    QQRTC             1
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
     C                   Z-ADD     *ZEROS        XXSEQ#
     C                   MOVE      QQSEQ#        XXSEQ#            5 0
     C*
     C*  INITIALIZE OUTPUT FOR SCREEN
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    RCM01
     C*
     C                   MOVEL     CMCOMM        SCCOMM
     C                   MOVEL     CMUSER        SCUSER
     C*
     C                   Z-ADD     CMCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCDAT
     C*
     C                   END
     C*
     C*   INITIALIZE ROUTINE * * *
     C*
     C                   EXSR      INIT
     C*
     C*   GET INITIAL USER SECURITY FILE RECORD
     C*
     C                   MOVE      Q1USER        XXNUSR
     C     SLKY          CHAIN     PSL01                              60
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
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C     *IN11         IFEQ      '1'
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C*
     C****************************************
     C*  F6 = USER PROFILE SEARCH           *
     C****************************************
     C     *INKF         IFEQ      '1'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C********** SLF06     IFEQ 'Y'
     C     SLF09         IFEQ      'Y'
      *** END   CHG *** 11/19/99 ***********************************************
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   CALL      'SC530R'
     C                   PARM                    SCNUSR           10
     C                   PARM                    QQRTCD            1
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END SCR01
     C*
     C                   END                                                    END SLF09=Y
     C                   GOTO      STRT
     C                   END                                                    END *INKF
     C*
     C****************************************
     C*  F7 = ADDITIONAL REMARKS CALL       *
     C****************************************
     C     *INKG         IFEQ      '1'
     C*
     C                   MOVEL     QQKEY         #7KEY            40
     C                   MOVE      XXSEQ#        #7SEQ#            5 0
     C                   MOVEL     CMPNAM        #7PNAM
     C                   CALL      'SC014R'
     C                   PARM                    #7KEY
     C                   PARM                    #7SEQ#
     C                   PARM                    #7PNAM
     C*
     C                   GOTO      STRT
     C                   END                                                    END *INKF
     C*
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C*
     C                   END                                                    END *INKL
     C*
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
     C*
     C*   PROCESS REQUESTED SCREEN
     C*
      *------------------------*
     C     SCR01T        TAG
      *------------------------*
     C*
     C*   EDIT THE USER FIELD FOR NEW DIARY INFORMATION
     C*
     C     SCNUSR        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVEL     SCNUSR        XXNUSR
     C     SLKY          CHAIN     PSL01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   VALIDATE THE DATE FIELD
     C*
     C     SCNDAT        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   ELSE
     C*
     C                   Z-ADD     SCNDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCNDAT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  ALL EDITS HAVE PASSED, MOVE VALUES INTO COMMENT STRING TO ADD
     C*     TO ADDITIONAL REMARKS FILE. (SPECIFYING DIARY COMPLETION)
     C*
     C                   MOVEL     CMT(1)        @@CMT
     C****BEGIN CHG****07/20/98***************************************
     C**********           MOVELCMUSER    @@USER
     C                   MOVEL     Q1USER        @@USER
     C****END CHG******07/20/98***************************************
     C                   MOVEL     XXCM          @@CM
     C                   MOVEL     XXCD          @@CD
     C                   MOVEL     XXCY          @@CY
     C                   MOVEL     XXSM          @@SM
     C                   MOVEL     XXSD          @@SD
     C                   MOVEL     XXSY          @@SY
     C*** BEGIN DEL *** 04/25/00 **************************************
     C********
     C***GET*NEXT AVAILABLE SEQUENCE NUMBER
     C********
     C********             EXSR ASEQ
     C********
     C***MOVE*VALUES INTO FIELDS FOR WRITE TO ADDITIONAL REMARKS RCD
     C********
     C********             MOVEL@@CMT     ARCM75
     C********
     C********             MOVELQQKEY     ARKEY
     C********             MOVE XXSEQ#    ARSEQ#
     C********             MOVE XXNXT#    ARASQ#
     C********             MOVELCMCSID    ARCSID
     C********             MOVE CMCDAT    ARCDAT
     C********             MOVELCMCODE    ARCODE
     C********             MOVELCMPNAM    ARPNAM
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
     C********             MOVELQ1USER    ARIUSR
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARIDT
     C********
     C********             MOVELCMRMCD    ARRMCD
     C********
     C********             WRITEPAR01
     C*** END DEL *** 04/25/00 ****************************************
     C*** BEGIN ADD *** 04/25/00 **************************************
     C*
     C*          Call API to write a DARTS Additional Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QQKEY         Q#KEY            40
     C                   MOVE      CMSEQ#        Q#SEQ#            5
     C                   MOVEL(P)  @@CMT         Q#CM75           75
     C                   MOVEL(P)  CMPNAM        Q#PNAM           10
     C                   MOVE      CMSECL        Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  CMRMCD        Q#RMCD            5
     C                   MOVE      '00000'       Q#ASQ#            5
     C                   MOVEL(P)  CMCSID        Q#CSID            2
     C                   MOVE(P)   CMCDAT        Q#CDAT            7
     C                   MOVE(P)   CMCODE        Q#CODE            2
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
     C                   PARM                    Q#CODE
     C*** END ADD *** 04/25/00 ****************************************
     C*
     C*  UPDATE REMARK DETAIL RECORD WITH NEW USER AND DATE
     C*
     C     CMKY          CHAIN     PCM01                              60
     C*
     C                   MOVEL     SCNUSR        CMUSER
     C*
     C                   Z-ADD     SCNDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMCDAT
     C*
     C*
     C**** BEGIN ADD ***** 07/21/98 ***********************************
     C                   MOVE      *BLANK        CMFL04
     C**** END   ADD ***** 07/21/98 ***********************************
     C                   UPDATE    PCM01
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C*
     C     INIT          BEGSR
     C                   TIME                    DSDTTM
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   ENDSR
     C*
     C*** BEGIN DEL *** 04/25/00 **************************************
     C*====================================================================*
     C********SUBROUTINE FOR GETTING THE NEXT AVAILABLE ASEQ#            ***
     C*====================================================================*
     C********
     C********   ASEQ      BEGSR
     C********             Z-ADD*ZEROS    XXASQ#
     C********   ARKY      SETLLPAR01
     C********   ARKY2     READEPAR01                    60
     C********
     C********   *IN60     DOWEQ'0'
     C********             Z-ADDARASQ#    XXNXT#
     C********   ARKY2     READEPAR01                    60
     C********             END
     C********
     C********             ADD  1         XXNXT#
     C********
     C********             ENDSR
     C*** END DEL *** 04/25/00 ****************************************
     C*
     OPCM01     E            RCM01
** ADDITIONAL COMMENT TABLE
DIARY COMPLETED BY:            DIARY DT:   /  /   DIARY COMPLETION:  /  /
