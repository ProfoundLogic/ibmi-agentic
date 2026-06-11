     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      *****************************************************************
     H*StoneRiver is a trademark of StoneRiver, Inc.
      *****************************************************************
     H/TITLE SNXTDCID - RETRIEVE NEXT DOCUMENT ID AND FOLDER AVAILABLE
     H/TITLE            FOR A GIVEN PREFIX.
      *****************************************************************
      *  Program Description:
      *  PROGRAM ACCEPTS PREFIX ID AS A PARM AND RETURNS THE NEXT AVAILABLE
      *  DOCUMENT ID AND THE GENERATED FOLDER.
      *****************************************************************
      * REVISIONS:
     H*                                                               *
     H*  10/17/11 C:278 Project: SY195                                *
     H*                 QDLS migration to IFS                         *
     H*                 Add Two Parameters with command.              *
     H*                                                               *
     H*  09/03/08 C:33 Seq#: WA099/65052                              *
     H*                Check the folder prior to checking the document*
     H*                                                               *
     H*  02/19/08 C:65 Project: SUPPORT(GENERAL)                      *
     H*                ADDING CHKDOC AND CHKFLR COMMANDS TO DETERMINE *
     H*                IF NEXT AVAILABLE DOCUMENT OR FOLDER IS        *
     H*                ACTUALLY AVAILABLE BEFORE USING.               *
     H*                                                               *
     H*  02/13/07 C:65 Project: SUPPORT(GENERAL)                      *
     H*                BY RECOMMENDATION OF MG, CHANGING MAX DOC      *
     H*                COUNT PER FOLDER TO 5000 AS IBM RECOMMENDS     *
     H*                THIS FOR SERVER PERFORMANCE.                   *
     H*                                                               *
     H*  11/08/05 C:33 Project: 139WAJ                                *
     H*                Allow for assigning PDF names.                 *
     H*                                                               *
     H*  08/30/04 C:33 Seq#: WA109/43564                              *
     H*                Call appropriate program for prefix handling   *
     H*                                                               *
     H*  05/18/04 C:62 Project: SY100                                 *
     H*                Add additional logic to handle client folders  *
     H*                                                               *
     H*  04/17/02 C:69 Project: SY059                                 *
     H*                Add logic to generate a name for a word        *
     H*                document if flag for Launcher is on.           *
     H*                                                               *
     H* 08/17/00  C:65 PROJECT: 110AQ                                 *
     H*                CHANGE FOLDER NAMES TO ALLOW MULTIPLE ENVIRS.  *
     H*                IF THERE IS NO ENVIRONMENT NUMBER THEN USE OLD *
     H*                NAMING CONVENTION.                             *
     H*                                                               *
     H*  09/22/99 C:65 PROJECT: SUPPORT                               *
     H*                REMOVE CO# FROM THE KEY FOR THE FOLDER FILES.  *
      *
      * 09/28/98 C:62 PROJECT: MC098 14022
      *               CHECK THE DTAARAS BY CALLING CL SENVR.
      *
      * 06/10/98 C:62 PROJECT:MC177
      *               ADDED SUBSCRIBER NUMBER TO PARM LIST
      *
      * 11/05/97 C:65 PROJECT: 68AWX
      *               MODIFY THIS PROGRAM TO PREVENT MAX DOCUMENTS IN
      *               FOLDER ERROR FROM ENDING JOB
      *               (COMPLETELY RE-WRITTEN)
      *****************************************************************
     FSMFCL     IF   E           K DISK
     F*** BEGIN CHG *** 06/01/98 **************************************
     F*SDFDL   UF  E           K        DISK                      A
     FSDFDL1    UF A E           K DISK
     F*** END   CHG *** 06/01/98 **************************************
     F*

     D CMD             C                   CONST('SNDMSG MSG(')
     D MSG             C                   CONST('Important Message: -
     D                                      The subst-
     D                                     ring for this job i-
     D                                     s nearly full (all -
     D                                     the folders are nea-
     D                                     rly full of documen-
     D                                     ts).  On the next b-
     D                                     usiness day between-
     D                                      the hours of 8:00a-
     D                                     m to 5:00pm Eastern-
     D                                      please call PDS to-
     D                                      change your substr-
     D                                     ing.')
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** BEGIN DEL *** 09/28/98 **************************************
     D**********DAILY       DS
     D**********                              1  80 XXDATA
     D**********                              1   3 XXENVR
     D*** END   DEL *** 09/28/98 **************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  FOLD                   1     12
     D*** BEGIN CHG ***  08/17/00  ************************************
     D*********                               1   5 XXFLRS
     D  XXPREF                 1      2
     D  XXENV#                 3      5
     D  XXFLS                  1      5
     D*** END CHG *****  08/17/00  ************************************
     D  Q2FND                  6      8
     D  XXPER                  9      9
     D  XXFINC                10     12
     D                 DS
     D  DOCID                  1     12
     D  XXPRE                  1      2
     D  XX1ST                  3      8
     D  XXPER2                 9      9
     D  XX2ND                 10     12
     D*** BEGIN ADD *** 08/17/00 **************************************
     D                 DS
     D  XXSUF                  1      3
     D  SUF1                   1      1
     D  SUF2                   2      2
     D  SUF3                   3      3

     I*** END ADD ***** 08/17/00 **************************************
      *

     C     FCKY          KLIST
     C*** BEGIN DEL ***  09/22/99  ************************************
     C***********          KFLD           XXCO#
     C*** END DEL *****  09/22/99  ************************************
     C                   KFLD                    XXPRE
     C                   KFLD                    XXSTAT
      *
     C     FCKY2         KLIST
     C*** BEGIN DEL ***  09/22/99  ************************************
     C***********          KFLD           XXCO#
     C*** END DEL *****  09/22/99  ************************************
     C                   KFLD                    XXPRE
     C                   KFLD                    XXFLRS
     C*** BEGIN ADD *** 06/01/98 **************************************
     C                   KFLD                    XXSUB#
     C*** END   ADD *** 06/01/98 **************************************
     C                   KFLD                    XXFND
     C*
     C*** BEGIN DEL *** 06/01/98 **************************************
     C********** *NAMVAR   DEFN WDAILYA   DAILY
     C*** END   DEL *** 06/01/98 **************************************
     C*** BEGIN DEL ***  09/22/99  ************************************
     C*******    *LIKE     DEFN FDCO#     XXCO#
     C*** END DEL *****  09/22/99  ************************************
     C     *LIKE         DEFINE    FDFND         XXFND
     C*** BEGIN ADD *** 06/01/98 **************************************
     C     *LIKE         DEFINE    FDSUB#        XXSUB#
     C*** END   ADD *** 06/01/98 **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFOLD           80
     C                   PARM                    QQDCID           12
     C                   PARM                    QQPRE             2
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQRTCD            1
     C*** BEGIN ADD *** 06/01/98 **************************************
     C                   PARM                    QQSUB#            5
     C*** END   ADD *** 06/01/98 **************************************
     C*** BEGIN ADD *** 05/18/04 **************************************
     C                   PARM                    QQPFLR           80
     C*** END   ADD *** 05/18/04 **************************************
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
     C                   MOVE      '.'           XXPER
     C                   MOVE      '.'           XXPER2
     C                   MOVE      QQFND         Q2FND
     C*** BEGIN CHG ***  02/13/07  *************************************
     C*************        Z-ADD20000     MAXDOC 110
     C                   Z-ADD     5000          MAXDOC           11 0
     C*** END CHG *****  02/13/07  *************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C* RETRIEVE ENVIRONMENT AND RELEASE DATA AREA.
     C*
     C*** BEGIN DEL *** 09/28/98 **************************************
     C**********           IN   DAILY
     C*
     C**********           MOVE XXENVR    XXENV   3
     C*** END   DEL *** 09/28/98 **************************************
     C*** BEGIN ADD *** 09/28/98 **************************************
     C*
     C                   CALL      'SA007C'
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C                   MOVEL(P)  ##RTCD        XXENV             1
     C*
     C*** END   ADD *** 09/28/98 **************************************
     C*** BEGIN ADD *** 05/18/04 **************************************
     C*
     C* GET THE SYSTEM VALUES
     C*
     C                   CALL      'SA001R'
01   C                   PARM      *BLANKS       ##WCIF            1            WC INST
02   C                   PARM      *BLANKS       ##MCIF            1            MC INST
03   C                   PARM      *BLANKS       ##WCIN            1            WC INTERFACE
04   C                   PARM      *BLANKS       ##AR              1            MC AR INST
05   C                   PARM      *BLANKS       ##NTWK            1            NETWORK INST
06   C                   PARM      *BLANKS       ##UR              1            PRECERT INST
07   C                   PARM      *BLANKS       ##MED             1            MEDMGMT INST
08   C                   PARM      *BLANKS       ##EDI             1            EDI INST
09   C                   PARM      *BLANKS       ##WCDK            1            WC DARTS KEY
10   C                   PARM      *BLANKS       ##RHAB            1            REHAB INST
11   C                   PARM      *BLANKS       ##MSS             1            MS SUPPORT
12   C                   PARM      *BLANKS       ##FSCH            1            FEE SCHEDULE
13   C                   PARM      *BLANKS       ##PRVB            1            PROVIDER BILLS
14   C                   PARM      *BLANKS       ##NPWC            1            NON-PDS WC  LS
15   C                   PARM      *BLANKS       ##OS              1            OUTOME/SAVINGS
16   C                   PARM      *BLANKS       ##ARLS            1            Auto Release Flag
17   C                   PARM      *BLANKS       ##CCOD            1            Correct Coding Flag
18   C                   PARM      *BLANKS       ##CRVW            1            Code Review Flag
19   C                   PARM      *BLANKS       ##PROM            1            ProMatch Flag
20   C                   PARM      *BLANKS       ##MCRV            1            Medicare CR Flag
21   C                   PARM      *BLANKS       ##FREQ            1            Frequency Flag
22   C                   PARM      *BLANKS       ##CRAC            1            CR Auto Complete
23   C                   PARM      *BLANKS       ##PREO            1            CR Auto Complete
24   C                   PARM      *BLANKS       ##PARN            1            Par/Non-Par/Both
25   C                   PARM      *BLANKS       ##MGRI            1            Multi Group Restr
26   C                   PARM      *BLANKS       ##KCIF            1            Keep Claimant info
27   C                   PARM      *BLANKS       ##CURS            1            PBILLS entry cursor
28   C                   PARM      *BLANKS       ##PCIB            1            Prov Check in Batch
29   C                   PARM      *BLANKS       ##CLT#            3            Client Number
30   C                   PARM      *BLANKS       ##UCPF            1            UCR Percentile
31   C                   PARM      *BLANKS       ##DCF             1            Data Center
     C*** END   ADD *** 05/18/04 **************************************
     C*** BEGIN ADD *** 08/17/00 **************************************
     C*
     C                   CALL      'SA030R'
     C                   PARM      *BLANKS       ##SUF             3
     C*** BEGIN ADD *** 05/18/04 **************************************
     C                   PARM      *BLANKS       ##PFLR           80
     C*** END   ADD *** 05/18/04 **************************************
     C*
     C                   MOVE      ##SUF         XXSUF
     C*
     C     SUF1          IFEQ      *BLANK
     C     SUF2          OREQ      *BLANK
     C     SUF3          OREQ      *BLANK
     C                   MOVE      '000'         ##SUF
     C                   ENDIF
     C*
     C                   MOVEL     ##SUF         XXENV#            3
     C*
     C                   MOVE      QQPRE         XXPREF            2
     C*
     C                   MOVE      '000'         XXFINC
     C                   MOVE      FOLD          SVFOLD           12
     C*
     C*** END   ADD *** 08/17/00 **************************************
     C*
     C*
     C* CHAIN OUT TO THE SYSTEM FOLDER CONTROL FILE.
     C*
     C*** BEGIN DEL ***  09/22/99  ************************************
     C***********          MOVE QQCO#     XXCO#
     C*** END DEL *****  09/22/99  ************************************
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQPRE         XXPRE             2
     C*** BEGIN ADD *** 06/01/98 **************************************
     C*
     C                   Z-ADD     *ZEROS        XXSUB#
     C*
     C     Q1PRMS        IFGT      6
     C*
     C                   MOVE      QQSUB#        XXSUB#
     C*
     C                   ENDIF
     C*** END   ADD *** 06/01/98 **************************************
     C                   MOVE      'A'           XXSTAT            1
     C*
     C     FCKY          CHAIN     PFC01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL(P)  FCFLRS        XXFLRS            5
     C*** BEGIN ADD *** 08/17/00 **************************************
     C     ##SUF         IFEQ      '000'
     C*** BEGIN ADD *** 05/18/04 **************************************
     C     ##DCF         OREQ      'Y'
     C*** END   ADD *** 05/18/04 **************************************
     C                   MOVE      XXFLRS        XXFLS
     C                   MOVE      FOLD          SVFOLD
     C                   ENDIF
     C*** END   ADD *** 08/17/00 **************************************
     C*
     C                   ENDIF
     C*
     C     FCKY2         CHAIN     PFD01                              60
     C*
     C     *IN60         IFEQ      *ON
     C*
     C*** BEGIN DEL ***  09/22/99  ************************************
     C***********          Z-ADDXXCO#     FDCO#
     C*** END DEL *****  09/22/99  ************************************
     C                   MOVE      XXPRE         FDPRE
     C                   MOVE      XXFLRS        FDFLRS
     C*** BEGIN ADD *** 06/01/98 **************************************
     C                   Z-ADD     XXSUB#        FDSUB#
     C*** END   ADD *** 06/01/98 **************************************
     C                   Z-ADD     XXFND         FDFND
     C                   MOVE      '000'         XXFINC
     C                   MOVE      FOLD          FDFLR
     C                   MOVE      'T00'         XXFINC
     C                   MOVE      FOLD          FDFLRT
     C                   Z-ADD     0             FDCNT
     C                   Z-ADD     0             FDCNTT
     C                   MOVE      'A'           FDSTAT
     C                   MOVEL(P)  Q1USER        FDUSER
     C                   MOVEL(P)  Q1PGM         FDPGM
     C                   Z-ADD     DSTIME        FDTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FDDT
     C*
     C                   MOVEL(P)  Q1USER        FDIUSR
     C                   MOVEL(P)  Q1PGM         FDIPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FDIDT
     C*
     C                   WRITE     PFD01
     C     FCKY2         CHAIN     PFD01                              60
     C*
     C                   ENDIF
     C*** BEGIN ADD ***  02/19/08  ***********************************
     C                   MOVE      *BLANK        XXDONE            1
     C     XXDONE        DOWEQ     *BLANK
     C*
     C     FCKY2         CHAIN     PFD01                              60
     C*** END ADD *****  02/19/08  ***********************************
     C*
     C* ENSURE THAT THE DOCUMENT COUNTER FOR THE PROPER ENVIRONMENT
     C* IS NOT AT THE MAX.
     C*
     C*** BEGIN CHG *** 09/28/98 **************************************
     C********** XXENV     IFEQ 'LWC'
     C     XXENV         IFEQ      ' '
     C*** END   CHG *** 09/28/98 **************************************
     C*
     C* LIVE ENV
     C*
     C                   MOVE      FDFLR         FOLD
     C*** BEGIN ADD *** 09/28/98 **************************************
     C                   MOVE      XXFINC        XFCNT             3 0
     C*** END   ADD *** 09/28/98 **************************************
     C*
     C     FDCNT         IFGE      MAXDOC
     C*** BEGIN CHG *** 09/28/98 **************************************
     C********** XXFINC    OREQ *ZEROS
     C     XFCNT         OREQ      *ZEROS
     C*** END   CHG *** 09/28/98 **************************************
     C*
     C* WE'VE EXCEEDED THE DOCUMENT LIMIT FOR THIS FOLDER, INCREMENT.
     C*
     C*** BEGIN DEL *** 09/28/98 **************************************
     C**********           MOVE XXFINC    XFCNT   30
     C*** END   DEL *** 09/28/98 **************************************
     C*** BEGIN ADD ***  08/17/00  ************************************
     C                   MOVE      SVFOLD        FOLD
     C*** END ADD *****  08/17/00  ************************************
     C                   ADD       1             XFCNT
     C                   MOVE      XFCNT         XXFINC
     C                   MOVE      FOLD          FDFLR
     C                   MOVE      *OFF          *IN60
     C*
     C*  CREATE NEW FOLDER
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      *BLANK        QQRTC
     C*** BEGIN ADD *** 05/18/04 **************************************
     C                   MOVEL(P)  *BLANKS       QQIFLD
     C     ##PFLR        IFNE      *BLANKS
     C                   MOVEL(P)  ##PFLR        QQIFLD
     C                   ENDIF
     C*** END   ADD *** 05/18/04 **************************************
     C*
     C*** BEGIN CHG *** 05/18/04 **************************************
     C**********           CALL 'CRTFLRC'
     C**********           PARM           FDFLR
     C**********           PARM           FCDESC
     C**********           PARM           QQRTC   1
     C***  Begin Add  ***  08/30/04  **************************************
     C     QQIFLD        IFEQ      *BLANKS
     C                   CALL      'CRTFLRC'
     C                   PARM                    FDFLR
     C                   PARM                    FCDESC
     C                   PARM                    QQRTC             1
     C                   ELSE
     C***  End   Add  ***  08/30/04  **************************************
     C                   CALL      'CRTFLRC2'
     C                   PARM                    FDFLR
     C                   PARM                    FCDESC
     C                   PARM                    QQIFLD           80
     C                   PARM                    QQRTC             1
     C***  Begin Add  ***  08/30/04  **************************************
     C                   ENDIF
     C***  End   Add  ***  08/30/04  **************************************
     C*** END   CHG *** 05/18/04 **************************************
     C*
     C     QQRTC         IFEQ      ' '
     C*
     C                   MOVE      *ON           *IN60
     C*
     C                   ELSE
     C*
     C                   MOVE      XXFINC        XFCNT             3 0
     C                   ADD       1             XFCNT
     C                   MOVE      XFCNT         XXFINC
     C                   MOVE      FOLD          FDFLR
     C*
     C                   ENDIF                                                  QQRTC IFEQ
     C*
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     XFCNT         IFGE      990
     C*
     C* GENERATE A BIG IMPORTANT MESSAGE HERE THAT THE SUBSTRING IS
     C* FULL FOR THIS PREFIX AND NEEDS TO BE CHANGED TOMMORROW.
     C*
     C                   MOVE      *BLANKS       XXCMD
     C     CMD           CAT       '''':0        XXCMD           256
     C     XXCMD         CAT       MSG:0         XXCMD
     C     XXCMD         CAT       '''':0        XXCMD
     C     XXCMD         CAT       ')':0         XXCMD
     C     XXCMD         CAT       'TOUSR(':1    XXCMD
     C     XXCMD         CAT       Q1USER:0      XXCMD
     C     XXCMD         CAT       ')':0         XXCMD
     C                   Z-ADD     256           LENGTH
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    XXCMD
     C                   PARM                    LENGTH           15 5
     C*
     C                   ENDIF                                                  XXCNT
     C*
     C                   Z-ADD     0             FDCNT
     C*
     C                   ENDIF
     C*
     C                   ADD       1             FDCNT
     C*
     C                   UPDATE    PFD01
     C*
     C                   ELSE                                                   XXENV IFEQ
     C*
     C* TEST ENV
     C*
     C                   MOVE      FDFLRT        FOLD
     C*** BEGIN ADD *** 09/28/98 **************************************
     C                   MOVE      XXFINC        XFCNTT            2 0
     C*** END   ADD *** 09/28/98 **************************************
     C*
     C     FDCNTT        IFEQ      MAXDOC
     C*** BEGIN CHG *** 09/28/98 **************************************
     C********** XXFINC    OREQ *ZERO
     C     XFCNTT        OREQ      *ZEROS
     C*** END   CHG *** 09/28/98 **************************************
     C*
     C* WE'VE EXCEEDED THE DOCUMENT LIMIT FOR THIS FOLDER, INCREMENT.
     C*
     C*** BEGIN DEL *** 09/28/98 **************************************
     C**********           MOVE XXFINC    XFCNTT  20
     C*** END   DEL *** 09/28/98 **************************************
     C*** BEGIN ADD ***  08/17/00  ************************************
     C                   MOVE      SVFOLD        FOLD
     C                   MOVE      'T00'         XXFINC
     C*** END ADD *****  08/17/00  ************************************
     C                   ADD       1             XFCNTT
     C                   MOVE      XFCNTT        XXFINC
     C                   MOVE      FOLD          FDFLRT
     C                   MOVE      *OFF          *IN60
     C*
     C*  CREATE NEW FOLDER
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      *BLANK        QQRTC
     C*** BEGIN ADD *** 05/18/04 **************************************
     C                   MOVEL(P)  *BLANKS       QQIFLD
     C     ##PFLR        IFNE      *BLANKS
     C                   MOVEL(P)  ##PFLR        QQIFLD
     C                   ENDIF
     C*** END   ADD *** 05/18/04 **************************************
     C*
     C*** BEGIN CHG *** 05/18/04 **************************************
     C**********           CALL 'CRTFLRC'
     C**********           PARM           FDFLRT
     C**********           PARM           FCDESC
     C**********           PARM           QQRTC   1
     C                   CALL      'CRTFLRC2'
     C                   PARM                    FDFLRT
     C                   PARM                    FCDESC
     C                   PARM                    QQIFLD           80
     C                   PARM                    QQRTC             1
     C*** END   CHG *** 05/18/04 **************************************
     C*
     C     QQRTC         IFEQ      ' '
     C*
     C                   MOVE      *ON           *IN60
     C*
     C                   ELSE
     C*
     C                   MOVE      XXFINC        XFCNTT            2 0
     C                   ADD       1             XFCNTT
     C                   MOVE      XFCNTT        XXFINC
     C                   MOVE      FOLD          FDFLRT
     C*
     C                   ENDIF                                                  QQRTC IFEQ
     C*
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     XFCNTT        IFGE      90
     C*
     C* GENERATE A BIG IMPORTANT MESSAGE HERE THAT THE SUBSTRING IS
     C* FULL FOR THIS PREFIX AND NEEDS TO BE CHANGED TOMMORROW.
     C*
     C                   MOVE      *BLANKS       XXCMD
     C     CMD           CAT       '''':0        XXCMD           256
     C     XXCMD         CAT       MSG:0         XXCMD
     C     XXCMD         CAT       '''':0        XXCMD
     C     XXCMD         CAT       ')':0         XXCMD
     C     XXCMD         CAT       'TOUSR(':1    XXCMD
     C     XXCMD         CAT       Q1USER:0      XXCMD
     C     XXCMD         CAT       ')':0         XXCMD
     C                   Z-ADD     256           LENGTH
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    XXCMD
     C                   PARM                    LENGTH           15 5
     C*
     C                   ENDIF                                                  XXCNT
     C*
     C                   Z-ADD     0             FDCNTT
     C*
     C                   ENDIF
     C*
     C                   ADD       1             FDCNTT
     C*
     C                   UPDATE    PFD01
     C*
     C                   ENDIF                                                  XXENV IFEQ
     C*
     C* BUILD THE DOCMENT NAME HERE.
     C*
     C*** BEGIN CHG *** 09/28/98 **************************************
     C********** XXENV     IFEQ 'LWC'
     C     XXENV         IFEQ      ' '
     C*** END   CHG *** 09/28/98 **************************************
     C*
     C                   Z-ADD     FDCNT         XXNUM             9 0
     C*
     C                   ELSE
     C*
     C                   Z-ADD     FDCNTT        XXNUM
     C*
     C                   ENDIF
     C*
     C*** Begin chg *** 04/17/02 **************************************
     C                   CALL      'SA037R'
     C                   PARM                    USELNC            1
     C*
     C***  Begin Chg  ***  11/07/05  **************************************
     C********** USELNC    IFEQ 'Y'
     C**********           Z-ADDXXNUM     XXNUM6  60
     C**********           MOVE XXNUM6    XX1ST
     C**********           MOVE 'DOC'     XX2ND
     C**********           ELSE
     C**********           MOVELXXNUM     XX1ST
     C**********           MOVE XXNUM     XX2ND
     C**********           ENDIF
     C                   SELECT
     C     USELNC        WHENEQ    'Y'
     C                   Z-ADD     XXNUM         XXNUM6            6 0
     C                   MOVE      XXNUM6        XX1ST
     C                   MOVE      'DOC'         XX2ND
     C     QQRTCD        WHENEQ    'P'
     C                   Z-ADD     XXNUM         XXNUM6
     C                   MOVE      XXNUM6        XX1ST
     C                   MOVE      'PDF'         XX2ND
     C                   OTHER
     C                   MOVEL     XXNUM         XX1ST
     C                   MOVE      XXNUM         XX2ND
     C                   ENDSL
     C*** End   chg *** 04/17/02 **************************************
     C*
     C* SAVE THE RESULTS TO BE RETURNED
     C*
     C*
     C*** BEGIN CHG *** 05/18/04 **************************************
     C**********           MOVELFOLD      QQFOLD    P
     C     ##PFLR        IFEQ      *BLANKS
     C                   MOVEL(P)  FOLD          QQFOLD
     C                   ELSE
     C     ##PFLR        CAT(P)    '/':0         QQFOLD
     C                   CAT       FOLD:0        QQFOLD
     C                   ENDIF
     C*** END   CHG *** 05/18/04 **************************************
     C*** BEGIN ADD *** 05/18/04 **************************************
     C     Q1PRMS        IFGT      7
     C                   MOVEL(P)  ##PFLR        QQPFLR
     C                   ENDIF
     C*** END   ADD *** 05/18/04 **************************************
     C                   MOVE(P)   DOCID         QQDCID
     C***  Begin Add  ***  09/03/08  **************************************
     C*
     C* Check if folder exists
     C*
     C                   MOVEL(P)  FOLD          Q1DNAM
     C                   MOVEL(P)  ##PFLR        Q1FNAM
     C                   MOVE(P)   '*FLR'        Q1DTYP
     C                   MOVE      *BLANK        Q1ERR
     C*
     C                   CALL      'SRCHKDLO'
     C                   PARM                    Q1DNAM           12
     C                   PARM                    Q1FNAM           32
     C                   PARM                    Q1DTYP            4
     C                   PARM                    Q1ERR             1
     C***  Begin ADD  ***  10/17/11  **************************************
     C                   PARM      *BLANKS       Q1UNCN          255
     C                   PARM      *BLANKS       Q1FSF             1
     C***  End  ADD  ***  10/17/11  ***************************************
     C*
     C     Q1ERR         IFEQ      '1'
     C*
     C                   MOVEL(P)  *BLANKS       QQIFLD
     C     ##PFLR        IFNE      *BLANKS
     C                   MOVEL(P)  ##PFLR        QQIFLD
     C                   ENDIF
     C*
     C                   CALL      'CRTFLRC2'
     C                   PARM                    FOLD
     C                   PARM                    FCDESC
     C                   PARM                    QQIFLD           80
     C                   PARM                    QQRTC             1
     C*
     C                   ENDIF
     C***  End   Add  ***  09/03/08  **************************************
     C*** BEGIN ADD ***  02/19/08  ***********************************
     C*
     C* CHECK IF DOCUMENT ALREADY EXISTS.
     C*
     C                   MOVEL(P)  QQDCID        Q1DNAM
     C                   MOVEL(P)  QQFOLD        Q1FNAM
     C                   MOVE(P)   '*DOC'        Q1DTYP
     C                   MOVE      *BLANK        Q1ERR
     C*
     C                   CALL      'SRCHKDLO'
     C                   PARM                    Q1DNAM           12
     C                   PARM                    Q1FNAM           32
     C                   PARM                    Q1DTYP            4
     C                   PARM                    Q1ERR             1
     C***  Begin ADD  ***  10/17/11  **************************************
     C                   PARM      *BLANKS       Q1UNCN          255
     C                   PARM      *BLANKS       Q1FSF             1
     C***  End  ADD  ***  10/17/11  ***************************************
     C*
     C     Q1ERR         IFEQ      '1'
     C                   MOVE      'Y'           XXDONE
     C                   ENDIF
     C*
     C                   ENDDO                                                  XXDONE DOWEQ
     C*** END ADD *****  02/19/08  ***********************************
     C*
     C                   RETURN
     C*
     C                   MOVE      '1'           *INLR
