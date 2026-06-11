     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 10/17/11 C:279 Project: SY195                                 *
      *                  - Modify program to pass/return UNC path     *
      *                    field and IFS flag to called/calling       *
      *                    programs.                                  *
      *                                                               *
      * 01/06/09 C:062 PROJECT: 114IRZ                                *
      *                ADD BARCODE INDEXING                           *
      *                                                               *
      * 08/25/06 C:062 PROJECT: VI114 - 54184                         *
      *                Default the diary user.                        *
      *                                                               *
      * 03/15/06 C:132 PROJECT: IM120 - 51809                         *
      *                Fix Diary Flag Problems                        *
      *                                                               *
      * 08/17/05 C:062 PROJECT: IM129 47421                           *
      *                Correct the display of the bypass.             *
      *                                                               *
      * 06/14/04 C:062 PROJECT: 129WCS                                *
      *                Add process to allow both a data entry pgm     *
      *                and reference an image                         *
      *                                                               *
      * 02/19/04 C:136 SUPPORT: DR129 40553                           *
      *                Ensure that the summarcy code (CMSMCD) gets    *
      *                a value from the Remark Table (STCTP) so       *
      *                that the summary code will display all images. *
      *                                                               *
      * 01/30/02 C:128 PROJECT: DR169                                 *
      *                If remark type changed,change description.     *
      *                                                               *
      * 11/29/99 C:113 PROJECT: DR083                                 *
      *                Correct display of F8.                         *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
      * ------------------------------------------------------------- *
      *****************************************************************
      *
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
      *****************************************************************
      * FILE SPECIFICATIONS  * * *
      *****************************************************************
     FISII02F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSDCML     O    E             DISK
     FSMSLL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FSSYLF     IF   E           K DISK
     FWXERP1    IF   E             DISK    USROPN
     FSDCMLI    IF   E           K DISK
     F                                     RENAME(PCM01:PCM0I)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FIMDTL     IF   E           K DISK
     FIMBML     IF   E           K DISK
     FIMDNL2    IF   E           K DISK
     F*** BEGIN ADD *** 06/14/04 **************************************
     FSDAIP     O    E             DISK
     F*** END   ADD *** 06/14/04 **************************************
     F*** BEGIN ADD *** 01/06/09 **************************************
     FIMRAL     IF   E           K DISK
     F*** END   ADD *** 01/06/09 **************************************
      *
      *****************************************************************
      * EXCEPTION SPECIFICATIONS * * *
      *****************************************************************
      *

     D*
     D CK1             S             70    DIM(5) CTDATA PERRCD(1)              COMMAND KEYS - SCR01
     D QCA             S             55    DIM(1) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*
     D DN              S             75    DIM(4)                               DOCN's
     D NOLNK           C                   CONST('No queues available ')
     D MDOC            C                   CONST('To Darts:')
     D MHAS            C                   CONST('has arrived on')
     D MQUE            C                   CONST('Queue:')
     D MSEN            C                   CONST('User :')
     D MID             C                   CONST('&QUEUE  ID')
     D****
     D DDAT            C                   CONST('Date:')
     D DTIM            C                   CONST('Time:')
     D DREA            C                   CONST('** INDEXED **')
     D DHBR            C                   CONST('has been reassigned')
     D DTOQ            C                   CONST('to Queue:')
      *****************************************************************
      * INPUT SPECIFICATIONS  * * *
      *****************************************************************
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*** BEGIN ADD *** 01/06/09 **************************************
     D                 DS
     D  YYDKEY                 1     40
     D  YYCO#                  1      3
     D  YYFND                  4      6
     D*** END   ADD *** 01/06/09 **************************************
     D*
      * * *  FILE INFORMATION DATA STRUCTURE
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     IPCM0I
     I              CMKEY                       PIKEY
     I              CMSEQ#                      PISEQ#
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
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C     *LIKE         DEFINE    CMCDAT        XXDATE
     C     *LIKE         DEFINE    CMTRTM        XXTIME
     C     *LIKE         DEFINE    CMSEQ#        XXSEQ#
     C     *LIKE         DEFINE    DRAL02        OFCPGM                         ADD 04/06/92
     C     *LIKE         DEFINE    Q2RECF        HLRECF                         ADD 04/06/92
     C     *LIKE         DEFINE    CMKEY         DEKEY                          ADD 04/09/92
     C     *LIKE         DEFINE    CMCDAT        XXCDAT                         ADD 09/03/92
     C     *LIKE         DEFINE    CMCDAT        XXDSDT                         ADD 09/03/92
     C                   Z-ADD     0             C                 5 0
     C                   Z-ADD     0             C1                5 0
     C                   MOVE      'N'           XXDEWR            1            ADD 04/09/92
     C                   MOVE      *BLANKS       DERTCD            1            ADD 04/09/92
     C                   MOVE      *BLANKS       DECOM1          256            ADD 04/09/92
     C                   MOVE      *BLANKS       DECOM2           19            ADD 04/09/92
     C*
     C                   MOVE      *BLANKS       DEKEY                          ADD 04/09/92
     C*
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
     C                   KFLD                    SCFND             3 0
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
     C     CMKYA         KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    ##SEQ#            5 0
     C*
     C*
     C*   KEY LIST FOR THE REMARKS DETAIL RECORD
     C*
     C     CMKY          KLIST
     C                   KFLD                    QQKEY
     C*
     C     CMKY1         KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    XXDATE
     C                   KFLD                    XXTIME
     C                   KFLD                    XXSEQ#
     C*
     C     CMKY2         KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    @@CDAT            7 0
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
     C     ARKY          KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
     C*
     C*   KEY LIST FOR THE REMARK SUMMARY CODE FILE
     C*
     C*   KEY LIST FOR THE REMARK TYPE FILE
     C*
     C     CTKY          KLIST
     C                   KFLD                    CMRMCD
     C*
     C     CTKY1         KLIST
     C                   KFLD                    SCCODE            5
     C*
     C*
     C     DTKY          KLIST
     C                   KFLD                    SCDOCT
     C*
     C*
     C*
     C     BMKY1         KLIST
     C                   KFLD                    QQBCHI
     C*
     C*
     C     DNKY2         KLIST
     C                   KFLD                    QQDCID
     C*
     C*** BEGIN ADD *** 01/06/09 **************************************
     C     RAKY          KLIST
     C                   KFLD                    QQDCID
     C*** END   ADD *** 01/06/09 **************************************
     C*
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDCID           12                    DOCI
     C                   PARM                    QQPTH            40
     C                   PARM                    QQDOCT            8
     C                   PARM                    QQDDSC           40
     C                   PARM                    QQQID            10
     C                   PARM                    QQQDSC           40
     C                   PARM                    QQBCHI           12
     C                   PARM                    QQ#PGS            6 0
     C                   PARM                    QQSDAT            8 0
     C                   PARM                    QQSTIM            6 0
     C                   PARM                    QQSREP            8
     C                   PARM                    QQKEY            40
     C                   PARM                    QQSEQ#            5 0
     C                   PARM                    QQRTCD            1
      ***** BEG ADD ***** 10/17/11 ***********************************
     C                   PARM                    QQUNCN          255
     C                   PARM                    QQIFSP            1
      ***** END ADD ***** 10/17/11 ***********************************
     C*** BEGIN ADD *** 06/14/04 **************************************
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
32   C                   PARM      *BLANKS       ##AUTB            1            Auto Batch   Flag
33   C                   PARM      *BLANKS       ##DEIF            1            DE/Image Flag
     C*** END   ADD *** 06/14/04 **************************************
     C*
     C*** SAVE THE RTCD (F=FAX, I=IMG, " "= SCANNED IMAGE)
     C                   MOVEL     QQRTCD        QQDCT             1
     C*
      *** BEGIN CHG *** 08/17/05 ***********************************************
     C********** QQDCT     IFNE 'F'
     C     QQDCT         IFEQ      'F'
      *** END   CHG *** 08/17/05 ***********************************************
      *** BEGIN CHG *** 11/29/99 ***********************************************
     C**********           MOVE *ON       *IN73
     C                   MOVE      *OFF          *IN73
     C                   ELSE
     C**********           MOVE *OFF      *IN73
     C                   MOVE      *ON           *IN73
      *** END   CHG *** 11/29/99 ***********************************************
     C                   ENDIF
     C*
     C*
     C                   MOVEL     *BLANKS       QQRTCD
     C*
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
     C**********           MOVELQ2FILE    SCFILE           SET FORMAT/FILE
     C**********           MOVELQ2RECF    SCRECF            FOR DISPLAY
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
     C                   MOVEL     *BLANKS       QQRTCD
     C     *INKC         IFEQ      '1'
     C                   MOVE      '3'           QQRTCD
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
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*
     C****************************************
     C*  F6 = BYPASS (ONLY FOR USER QUEUE)   *
     C****************************************
     C***
     C     *INKF         IFEQ      *ON
     C*** BEGIN DEL *** 08/17/05 **************************************
     C********** *IN73     IFEQ *ON                         ONLY USR QUEUES
     C*** END   DEL *** 08/17/05 **************************************
     C*
     C*** BYPASS BY PUTTING A "P" IN THE RTNCD AND ENDING
     C                   MOVE      'P'           QQRTCD
     C                   GOTO      TERM
     C*
     C*** BEGIN DEL *** 08/17/05 **************************************
     C**********           ENDIF                           END 73
     C*** END   DEL *** 08/17/05 **************************************
     C                   ENDIF                                                  END KF
     C***
     C*
     C*
     C*
     C*
     C****************************************
     C*  F7 = Document Notes                *
     C****************************************
     C***
     C     *INKG         IFEQ      *ON
     C*
     C                   MOVEL     *BLANKS       QQRTCD
     C                   CALL      'IS016R'
     C                   PARM                    QQDCID
     C                   PARM      Q1PGM         XXPNAM           10
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  END KG
     C***
     C*
     C*
     C*
     C****************************************
     C*  F8 = Reject Document               *
     C****************************************
     C***
     C     *INKH         IFEQ      *ON
     C*** BEGIN DEL *** 08/17/05 **************************************
     C********** *IN73     IFEQ *OFF                        NO RJ FOR USERQS
     C*** END   DEL *** 08/17/05 **************************************
      *
      * * * Reject the Document being Indexed.  This will
      * * *  call the Reject Document program  to allow
      * * *  the entry of reason for the reject, Then update
      * * *  the IE record, and the DO record with RJ   status.
      * * *  Code will be RJ. (later a program will be available
      * * *  to see the "rejects" and perform some processing on them.
      * * *  The user will be required to fill in a reason for the
      * * *  Reject in the program, which will be written to the
      * * *  Doc Notes.
      *
     C                   CALL      'IS157R'
     C                   PARM                    QQDCID
     C                   PARM      'RJ'          QQDSTS            2
     C                   PARM      'ISII01R '    XXPNAM           10
     C                   PARM      ' '           QQRTC             1
     C*
     C     QQRTC         IFEQ      'A'
     C                   MOVE      '3'           QQRTCD
     C                   MOVEL     '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C     QQRTC         IFEQ      'B'
     C                   GOTO      STRT
     C                   END
     C*
      ***** NOW VALID RETURN FROM REJECT, BYPASS THE CURRENT RCD.
     C*
     C*
     C                   MOVEL     '8'           QQRTCD                         REASS. RTN
     C                   GOTO      TERM
     C*
     C*** BEGIN DEL *** 08/17/05 **************************************
     C**********           ENDIF                           END *IN73
     C*** END   DEL *** 08/17/05 **************************************
     C                   ENDIF                                                  END KH
     C***
     C****************************************
     C*  F9 = ReASSIGN DOCUMENT             *
     C****************************************
     C***
     C     *INKI         IFEQ      *ON
     C*
     C********************
     C*** RE-ASSIGN ***
     C******************** MOVE ' '       Q@RTC   1
     C                   MOVE      QQSDAT        #$SDT             8
     C                   MOVE      QQSTIM        #$STM             6
     C                   CALL      'IS115R'
     C                   PARM                    #$SDT
     C                   PARM                    #$STM
     C                   PARM                    QQDOCT            8
     C                   PARM                    QQDCID           12
     C                   PARM                    QQDCT
     C                   PARM      *BLANKS       @#FLR             8
     C                   PARM                    @#SFLR           12
     C                   PARM      QQPTH         @#PTH            63
     C                   PARM                    QQQID
     C                   PARM      QQDDSC        @#DDSC           30
     C                   PARM      *BLANKS       @#RTC1            1
     C*
     C     @#RTC1        IFEQ      'A'
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C     @#RTC1        IFEQ      'B'
     C                   MOVE      'C'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       QQKEY
     C                   MOVEL     '9'           QQRTCD                         REASS. RTN
     C                   GOTO      TERM
     C                   ENDIF                                                  END KH
     C*
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
     C     FLD           IFEQ      'SCDUSR  '
     C*
     C                   CALL      'SC530R'
     C                   PARM                    SCDUSR           10
     C                   PARM                    QQRTCD            1
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*
     C     SCDEPT        IFEQ      '? '
     C                   MOVEL     *BLANKS       SCCODE
     C*
     C                   MOVE      *BLANK        ZZRTCD
     C                   CALL      'SC580R'
     C                   PARM                    SCCODE
     C                   PARM                    ZZRTCD
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVEL     SCCODE        SCDEPT
     C                   MOVE      SCCODE        SCRMTY
     C     CTKY1         CHAIN     PCT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CTFL01        SCDITP
      *** Begin Chg *** 01/30/02 ***********************************************
     C                   MOVE      *BLANKS       SCCOMM
     C                   MOVEL     CTDESC        SCCOMM
      *** End   Chg *** 01/30/02 ***********************************************
     C                   END
     C                   GOTO      STRT
     C                   END                                                    SCCODE ='? '
     C*
     C                   MOVEL     SCPNAM        $$2A              2
     C     $$2A          IFEQ      '? '
     C                   MOVEL     *BLANKS       SCPNAM
     C*
     C                   MOVE      *BLANK        ZZRTCD
     C                   CALL      'SC590R'
     C                   PARM                    SCPNAM
     C                   PARM                    ZZRTCD
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    SCPNAM ='? '
     C*
     C     SCRMTY        IFEQ      '? '
     C                   MOVEL     *BLANKS       SCCODE
     C                   MOVEL     SCDEPT        SCCODE
     C                   MOVE      *BLANK        ZZRTCD
     C                   CALL      'SC580R'
     C                   PARM                    SCCODE
     C                   PARM                    ZZRTCD
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVEL     SCCODE        SCDEPT
     C                   MOVE      SCCODE        SCRMTY
     C     CTKY1         CHAIN     PCT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CTFL01        SCDITP
      *** Begin Chg *** 01/30/02 ***********************************************
     C                   MOVE      *BLANKS       SCCOMM
     C                   MOVEL     CTDESC        SCCOMM
      *** End   Chg *** 01/30/02 ***********************************************
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
     C***********
     C*  SCR01  *
     C***********
     C                   MOVEL     *BLANKS       QQRTCD
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      'C'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
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
     C                   MOVE      *ZERO         XXFND
     C                   MOVE      *ZERO         SCCO#
     C                   MOVE      *ZERO         SCFND
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*****************************************************************
     C** INKN - FUNCTION 14 - SPLIT REQUEST
     C*****************************************************************
     C*
     C*   PROCESS SPLIT REQUEST
     C*
     C     *INKN         IFEQ      '1'
     C*
     C*
     C                   MOVE      QQSDAT        #$SDT             8
     C                   MOVE      QQSTIM        #$STM             6
     C*
     C     QQDCT         IFEQ      'F'
     C                   MOVEL     'F'           Q@RTC             1
     C                   ELSE
     C                   MOVEL     'I'           Q@RTC
     C                   ENDIF
     C*
     C                   MOVEL     'I'           Q@RTX
     C                   CALL      'IS215R3'
     C                   PARM      QQQID         $$QID            10
     C                   PARM      QQDCID        $$DCID           12
     C                   PARM                    #$SDT             8
     C                   PARM                    #$STM             6
     C                   PARM      QQDOCT        $$DOCT            8
     C                   PARM                    Q@RTX             1             FRM INDX PG
     C*
     C     Q@RTX         IFEQ      '3'
     C                   MOVEL     Q@RTX         QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*
     C     Q@RTX         IFEQ      '*'
     C                   ENDIF
     C*
     C*
     C     Q@RTX         CABEQ     'C'           STRT                             F12
     C*
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*
     C*****************************************************************
     C** INKQ - FUNCTION 16 - NEW INDEX PROGRAM
     C*****************************************************************
     C*
     C*   PROCESS INDEX CHANGE
     C*
     C     *INKQ         IFEQ      '1'
     C*
     C                   MOVEL     'K'           QQRTCD
     C*
     C                   GOTO      TERM
     C                   END
     C*
     C****************************************
     C*  F17 = USER PROFILE SEARCH           *
     C****************************************
     C     *INKR         IFEQ      '1'
     C     SLF09         IFEQ      'Y'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   CALL      'SC530R'
     C                   PARM                    SCDUSR           10
     C                   PARM                    ZZRTCD            1
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END SCR01
     C*
     C                   END                                                    END SLF09=Y
     C                   GOTO      STRT
     C                   END                                                    END *INKI
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
     C*#*
     C     C1            IFEQ      1
     C     C1            OREQ      5
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
     C*#*
     C     *IN73         IFEQ      *ON
     C                   Z-ADD     5             C1
     C                   ELSE
     C                   Z-ADD     1             C1
     C                   ENDIF
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
     C*
     C                   EXSR      EDTS01                                        EDIT SCR01 FIELDS
     C*
     C*** WRITE REMARK RECORD
     C*
     C*  *** MOVE SCREEN KEY INTO CM RCD
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
     C* * * EXSR HERE TO WRITE DOC NOTES, AND ALSO TO COPY ALL DOC NOTE
     C* * *  TO THE SMARP FILE FOR THIS NEW DART REMARK
     C*
     C                   EXSR      DNOT
     C*
     C*
     C*** RETAIN THE ITEM ON THE USERS QUEUE?
     C     SCRETN        IFEQ      'Y'
     C                   MOVEL     'R'           QQRTCD
     C                   ENDIF
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
     C                   Z-ADD     0             XXTIME
     C                   Z-ADD     0             XXDATE
     C                   Z-ADD     0             XXSEQ#
     C                   Z-ADD     *HIVAL        ##SEQ#
     C                   Z-ADD     0             ##SECL
     C                   MOVE      'N'           ##SBYL
     C*** GET USER PARM
     C     UPKEY         CHAIN     PUP01                              60
     C                   MOVE      UPCO#         XXCO#             3 0
     C                   MOVE      UPFND         XXFND             3 0
     C                   MOVE      UPFND         SCFND             3 0
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
     C*** BEGIN ADD *** 01/06/09 **************************************
     C*
     C* TRY TO RETRIEVE DATA FROM IMRAP
     C*
     C     RAKY          CHAIN     PRA01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RADKEY        YYDKEY
     C                   MOVE      YYCO#         XXCO#
     C                   MOVE      YYFND         XXFND
     C                   MOVE      YYCO#         SCCO#
     C                   MOVE      YYFND         SCFND
     C                   ENDIF
     C*** END   ADD *** 01/06/09 **************************************
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C*************        MOVE XXFND     SCFND
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
      *#*
     C     *IN73         IFEQ      *ON
     C                   Z-ADD     5             C1
     C                   ELSE
     C                   Z-ADD     1             C1
     C                   END                                                    END DCT = F
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
     C                   MOVEL     QQDCID        SCDOCI
     C                   MOVEL     QQPTH         SCPTH
     C                   MOVEL     QQDOCT        SCDOCT
     C                   MOVEL     QQDDSC        SCDDSC
     C                   MOVEL     QQQID         SCQID            10
     C                   MOVEL     QQQDSC        SCQDSC           40
     C                   MOVEL     QQBCHI        SCBCHI
     C                   MOVE      QQ#PGS        SC#PGS
     C                   MOVE      QQSDAT        SCSDAT
     C                   MOVE      QQSTIM        SCSTIM
     C                   MOVEL     QQSREP        SCSREP
     C*
     C******************************************************************
     C*
     C                   MOVEL     SCDDSC        SCCOMM
     C                   MOVEL     'N'           SCARMK
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        SCCDAT
     C*
     C*
     C     DTKY          CHAIN     PDT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     DTRMCD        SCDEPT
     C                   MOVE      DTRMCD        SCRMTY
     C                   ELSE
     C                   MOVEL     *BLANKS       SCDEPT
     C                   MOVE      *BLANKS       SCRMTY
     C                   ENDIF
     C*
      ***** BEG ADD ***** 03/15/06 ***************************************
      * FILL IN THE DEFAULT DIARY TYPE HERE
     C                   MOVEL     SCDEPT        SCCODE
     C                   MOVE      SCRMTY        SCCODE
     C     CTKY1         CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CTFL01        SCDITP
     C                   ELSE
     C                   MOVEL     *BLANKS       SCDITP
     C                   ENDIF                                                  *IN60  = *OFF
      ***** END ADD ***** 03/15/06 ***************************************
      ***** BEG ADD ***** 08/25/06 ***************************************
     C*
     C* DEFAULT THE DIARY USER TO CURRENT USER IF THE DIARY FLAG IS Y
     C*
     C     SCDITP        IFEQ      'Y'
     C                   MOVEL     Q1USER        SCDUSR
     C                   ENDIF
      ***** end ADD ***** 08/25/06 ***************************************
     C*
     C     BMKY1         CHAIN     PBM01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     BMBDSC        SCBDSC           40
     C                   ELSE
     C                   MOVEL     *BLANKS       SCBDSC
     C                   ENDIF
     C*
     C                   MOVEL     SCBDSC        SCCOMM
     C*
     C** ** ANY NON-SYSTEM ** **  DOC NOTES EXIST????
     C     DNKY2         CHAIN     PDN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN69
     C                   ELSE
     C                   MOVE      *OFF          *IN69
     C                   ENDIF
     C*
     C*
     C******************************************************************
     C*
     C                   ENDSR
     C*
      *====================================================================*
      * * *   WCOM  SUBROUTINE - WRITE REMARK RECORD           W C O M   ***
      *====================================================================*
     C*
     C     WCOM          BEGSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT              7 0
     C                   Z-ADD     DSTIME        HHTIME            6 0
     C*
     C*** GET LAST SEQ# USED
     C*
     C* * * WRITE REMARKS  TO FILE * * *
     C*
     C                   MOVE      ##SECL        CMSECL
     C                   MOVE      ##SBYL        CMSBYL
     C*
     C                   MOVEL     ##KEY         CMKEY
     C                   MOVEL     *BLANKS       CMCSID                         ANY LONGER
     C                   MOVEL     ##PNAM        CMPNAM
     C                   MOVE      *BLANKS       CMCODE
     C                   MOVEL     SCCODE        CMRMCD
     C*** BEGIN ADD *** 06/14/04 **************************************
     C                   MOVE      ' '           CMFL01
     C*** END   ADD *** 06/14/04 **************************************
      *** BEG ADD ***** 02/19/04 **********************************
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      CTSMCD        CMSMCD
     C*** BEGIN ADD *** 06/14/04 **************************************
     C     ##DEIF        IFEQ      'Y'
     C     CTDEPG        ANDNE     *BLANKS
     C                   MOVE      'Y'           CMFL01
     C                   ENDIF
     C*** END   ADD *** 06/14/04 **************************************
     C                   ELSE
     C                   MOVE      *BLANKS       CMSMCD
     C*** BEGIN ADD *** 06/14/04 **************************************
     C                   MOVE      *BLANKS       CTDEPG
     C*** END   ADD *** 06/14/04 **************************************
     C                   ENDIF
      *** END ADD ***** 02/19/04 **********************************
     C                   MOVEL     '***IMAGE'    CMDCID
     C                   MOVE      '*** '        CMDCID
     C                   MOVEL     SCCOMM        CMCOMM
     C*** BEGIN DEL *** 06/14/04 **************************************
     C**********           MOVE ' '       CMFL01
     C*** END   DEL *** 06/14/04 **************************************
     C****   LOAD FAX FOLDER HERE *********************
     C                   MOVEL     QQPTH         CMFOLD
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMCDAT
     C                   Z-ADD     HHDT          CMTRDT
     C                   Z-ADD     HHTIME        CMTRTM
      ***** BEG ADD ***** 10/17/11 ***********************************
     C                   MOVEL     QQUNCN        CMUNCN
     C                   MOVEL     QQIFSP        CMIFSP
      ***** END ADD ***** 10/17/11 ***********************************
     C*
     C*
     C*
     C***** FILL HIDDEN FIELDS ****
     C*
     C* * * IMAGE FIELDS
     C                   MOVEL     SCIQUE        CMIQUE                             HIDDEN
     C                   MOVEL     SCIPTH        CMIPTH                             HIDDEN
     C                   MOVE      SCPIDX        CMPIDX                             HIDDEN
     C                   MOVEL     SCAPID        CMAPID                             HIDDEN
     C                   MOVEL     QQDCID        CMIDOC                             HIDDEN
     C                   MOVEL     SCQPTY        CMQPTY                             HIDDEN
     C                   MOVEL     SCDCTP        CMDCTP                             HIDDEN
     C*** FAX FLAG
     C     QQDCT         IFNE      'F'
     C                   MOVEL     'Y'           CMIMGF                             HIDDEN
     C                   ELSE
     C                   MOVEL     'F'           CMIMGF
     C                   MOVEL     QQDCID        CMDCID
     C                   MOVE      *BLANKS       CMIDOC
     C                   ENDIF
     C*
     C*
     C*
     C                   MOVEL     *BLANK        CMSTAT
     C                   MOVEL     Q1PGM         CMPGM
     C*
     C*** IF DIARY USER ENTERED, WRITE TO CMUSER
     C     SCDUSR        IFNE      *BLANKS
     C                   MOVEL     SCDUSR        CMUSER
     C                   ELSE                                                   ELSE NE *BLANKS
     C*** ELSE DEFAULT TO CURRENT USER
     C                   MOVEL     Q1USER        CMUSER
     C                   END                                                    END NE *BLANKS
     C*
     C                   MOVEL     Q1USER        CMIUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C     SCDITP        IFEQ      'Y'
     C     SCDITP        OREQ      'N'
     C*
     C                   MOVE      SCDITP        CMDIFL
     C                   ELSE
     C                   MOVE      ' '           CMDIFL
     C                   END
     C*                                                    DFT LOC=DASD
     C                   MOVEL     'D'           CMFL05                         ADD 05/11/93
     C*
     C                   MOVE      SCPGFL        CMFL02                         ADD 03/20/92
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMIDT
     C*
     C                   Z-ADD     *ZERO         CMDCDT
     C                   MOVEL     Q1PGM         CMIPGM                         ADD 10/14/91
     C*
      *** BEG DEL ***** 02/19/04 *****************************************
     C******************** MOVE *BLANKS   CMSMCD
      *** END DEL ***** 02/19/04 *****************************************
     C                   MOVE      *BLANKS       CMFL04
     C                   MOVE      *BLANKS       CMFL05
     C*
     C*** GET LAST SEQ# USED
     C*
     C                   MOVEL     SCKEY         QQKEY            40
     C*
     C     CMKYA         SETGT     PCM0I
     C                   READP     PCM0I                                  60
     C     *IN60         IFEQ      '0'
     C     QQKEY         ANDEQ     PIKEY
     C     PISEQ#        ADD       1             CMSEQ#
     C                   ELSE
     C                   Z-ADD     0             CMSEQ#
     C                   END
     C*
     C                   WRITE     PCM01
     C*** BEGIN ADD *** 06/14/04 **************************************
     C     ##DEIF        IFEQ      'Y'
     C                   EXSR      WRTAIP
     C                   ENDIF
     C*** END   ADD *** 06/14/04 **************************************
     C                   MOVEL     *BLANKS       QQRTCD
     C*
     C                   MOVE      CMSEQ#        QQSEQ#
     C*
     C*** IF ADDITIONAL RMRKS ARE REQUESTED (SCARMK='Y') THEN CALL PROGRAM T
     C*** TO ADD THE REMARKS.
     C*
     C                   MOVEL     'ISII02R '    QQPNAM
     C     SCARMK        IFEQ      'Y'
     C                   CALL      'SC014R'
     C                   PARM                    QQKEY
     C                   PARM                    CMSEQ#
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
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              21
     C     *IN21         CABEQ     '1'           STRT
     C*
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
     C*
     C*
     C*  Check to see if a recursive call was made.  If so... error msg
     C*
     C                   MOVE      *BLANKS       XXPGM            10
     C                   MOVEL     CTDEPG        XXPGM
     C                   MOVE      *BLANKS       SCDCID
     C                   MOVEL     CTDEPG        SCDCID
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDSDT
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
     C     XXCDAT        IFLE      XXDSDT
     C*
     C     ERRFLE        IFEQ      ' '
     C                   CALL      'ERROR1C'
     C*
     C                   Z-ADD     55            LENGTH           15 5
     C                   CALL      'QCMDEXC'                              37
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH
     C*
     C                   OPEN      WXERP1
     C                   MOVE      'Y'           ERRFLE            1
     C                   END
     C*
     C     7             CHAIN     PER01                              37
     C     *IN37         DOWEQ     '0'
     C*
     C*  Check for Recursive call to program
     C*
     C     ERPGM         IFEQ      XXPGM
     C                   WRITE     SCR99
     C                   WRITE     SCR01
     C                   MOVE      '1'           *IN47
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   READ      PER01                                  37
     C                   END
     C*
     C*  ALL EDITS PASSED - CALL DATA ENTRY PROGRAM
     C*  (NOTE: It was necessary to break up the comment field of size
     C*   275 into DECOM1 and DECOM2 because the largest alpha-numeric
     C*   value allowed for a parameter is 256.
     C*
     C                   MOVE      *BLANKS       DEKEY
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
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDSDT
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
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
     C                   MOVEL     *BLANK        ZZRTCD            1
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
     C                   PARM                    ZZRTCD
     C*
     C     ZZRTCD        CABEQ     '1'           STRT                     54
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
     C*
      *====================================================================*
      * * *   DNOT DOC NOTES  - CREATE DOC NOTES AND CPY NOTES TO SMAR* * **
      *====================================================================*
     C     DNOT          BEGSR
      *
     C*** FILL ALL DOC NOTE FIELDS
     C** GET CURRENT DATE/TIME TO CREATE NOTES...
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        #$NOWD            8
     C                   MOVE      DSTIME        #$NOWT            6
      *
     C                   MOVE      *BLANKS       XX75             75
     C                   MOVEL     #$NOWD        XXM               2
     C                   MOVE      #$NOWD        XXY               4
     C     2             SUBST     #$NOWD:3      XXD               2
     C     XXM           CAT(P)    '/'           XX3A              3
     C     XX3A          CAT(P)    XXD           XX5A              5
     C     XX5A          CAT(P)    '/'           XX6A              6
     C     XX6A          CAT(P)    XXY           XXMDYY           10
     C*
     C                   MOVEL     #$NOWT        XXH               2
     C                   MOVE      #$NOWT        XXS               2
     C     2             SUBST     #$NOWT:3      XXM               2
     C     XXH           CAT(P)    ':'           XX3A              3
     C     XX3A          CAT(P)    XXM           XX5A              5
     C     XX5A          CAT(P)    ':'           XX6A              6
     C     XX6A          CAT(P)    XXS           XXHMS             8
      *
     C     DDAT          CAT(P)    XXMDYY:1      XX75
     C                   CAT(P)    DTIM:1        XX75
     C                   CAT(P)    XXHMS:1       XX75
     C                   CAT(P)    DREA:5        XX75
     C*
     C                   MOVEL     XX75          DN(1)
     C*
     C                   MOVE      CMSEQ#        $XSEQ             5
     C     MDOC          CAT(P)    CMKEY:1       XX75
     C                   CAT(P)    $XSEQ:5       XX75
     C*
     C                   MOVEL     XX75          DN(2)
     C*
     C     MSEN          CAT(P)    Q1USER:1      XX75
     C                   CAT(P)    '.'           XX75
     C                   MOVEL     XX75          DN(3)
     C                   MOVEL     *BLANKS       DN(4)
      *
     C*
     C*** CALL PGM TO WRITE DOC NOTES
     C*
      * * * DOW LOOP HERE TO PROCESS MULTIPLE ITEMS.  SETUP ITEMS
      * * * AS COMMENTS, OR ARRAY ELEMENTS
     C*
     C*
     C                   DO        4             G                 3 0
     C*
     C                   MOVEL     QQDCID        $#DCID           12
     C                   MOVEL     DN(G)         QQCM75
     C*
     C     G             IFGE      4
     C                   MOVE      '1'           Q$RTC             1
     C                   ELSE
     C                   MOVE      ' '           Q$RTC
     C                   ENDIF
     C*
     C                   CALL      'ISDOCNR'
     C                   PARM                    QQCM75           75              COMMENT
     C                   PARM                    QQDCID           12              DOCID
     C                   PARM                    #$NOWD                           NOTE DATE
     C                   PARM      Q1PGM         QQPNAM           10              CALLING PGM
     C                   PARM      'H'           QQPRFL            1              PROTECT FLG
     C                   PARM                    Q$RTC             1              RTN CODE
     C*
     C                   ENDDO
     C*
     C*
     C* * * Now we need to create the Additional Remarks for this
     C* * *  DART, by read/writing all Notes to the SMARP file.
     C* * *  We will also BACK DATE the Remarks, so they are NOT modifi
     C* * *  able.
     C*
     C                   CALL      'ISNOTAR'
     C                   PARM                    QQDCID           12
     C                   PARM      CMKEY         Q@KEY            40
     C                   PARM      CMSEQ#        Q@SEQ#            5 0
     C                   PARM      *BLANKS       Q@RCD             1
     C*
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 06/14/04 **************************************
     C*====================================================================*
     C* WRTAIP - WRITE SDAIP                                            * **
     C*====================================================================*
     C     WRTAIP        BEGSR
     C*
     C                   CLEAR                   PAI01
     C*
     C                   MOVEL(P)  CMKEY         AIKEY
     C                   Z-ADD     CMSEQ#        AISEQ#
     C*
     C                   MOVEL(P)  CTDEPG        AIDPGM
     C*
     C     AIDPGM        IFNE      *BLANKS
     C                   MOVEL(P)  CMKEY         AIDKEY
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL(P)  Q1USER        AIUSER
     C                   MOVEL(P)  Q1PGM         AIPGM
     C                   Z-ADD     DSTIME        AITIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AIDT
     C*
     C                   MOVEL(P)  Q1USER        AIIUSR
     C                   MOVEL(P)  Q1PGM         AIIPGM
     C                   Z-ADD     DSTIME        AIITIM
     C                   Z-ADD     AIDT          AIIDT
     C*
     C                   WRITE     PAI01
     C*
     C                   ENDSR
     C*** END   ADD *** 06/14/04 **************************************
     C/EJECT
**  CK1 - Command Keys - SCR01
F3=Exit  F5=Search   F7=Notes   F8=Reject  F9=Reassign   F24=More keys
F10=Search         F12=Previous   F13=Chg User Parms     F24=More keys
F14=Split    F16=New Index Type    F23=Secure Remarks    F24=More keys
F14=Split    F16=New Index Type    F23=Unsecure Remarks  F24=More keys
F3=Exit  F5=Search  F6=Bypass  F7=Notes   F9=Reassign    F24=More keys
**  QCMDEXC TABLE
OVRDBF FILE(WXERP1) TOFILE(QTEMP/WXERP1 )
