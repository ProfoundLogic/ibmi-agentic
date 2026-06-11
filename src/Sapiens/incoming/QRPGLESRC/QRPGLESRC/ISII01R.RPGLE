     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')

      *****************************************************************
      * StoneRiver is a trademark of StoneRiver, Inc.
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 04/10/19 C:062 Project: 152WAB                                *
      *                Apply CSI mods for S1 Medical.                 *
      *                                                               *
      * 04/21/15 C:302 PHW 33196                                      *
      *                Correct command keys description.              *
      *                Clean up program.                              *
      *                                                               *
      * 03/06/13 C:062 Redmine: 2483                                  *
      *                Add code to prevent duplicate darts remarks    *
      *                                                               *
      * 10/17/11 C:279 Project: SY195                                 *
      *                  - Modify program to pass/return UNC path     *
      *                    field and IFS flag to called/calling       *
      *                    programs.                                  *
      *                                                               *
      * 01/06/09 C:062 PROJECT: 114IRZ                                *
      *                ADD BARCODE INDEXING                           *
      *                                                               *
      * 07/17/08 C:062 PROJECT: 64405                                 *
      *                Add client condition                           *
      *                                                               *
      * 07/24/07 C:132 PROJECT: 114IOT                                *
      *                Add process to display window to verify emp#   *
      *                to be indexed, with option to cancel out.      *
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
      * INDICATOR USAGE                                               *
      * SCR01:                                                        *
      * N98      EITHER NO SECURITY OR MEETS SECURITY LEVEL           *
      *  98      PROTECT REMARK TYPE BECAUSE OF SECURITY LEVEL        *
      *  99      GLOBAL - IMAGE PRODUCT IS INSTALLED                  *
      *****************************************************************

     FISII01F   CF   E             WORKSTN
     F                                     Handler('PROFOUNDUI(HANDLER)')
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
     FWMEML     IF   E           K DISK
     FIMDTL     IF   E           K DISK
     FIMBML     IF   E           K DISK
     FIMDNL2    IF   E           K DISK
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM0P)
     FSDAIP     O    E             DISK
     FIMRAL     IF   E           K DISK

      *****************************************************************
      * EXCEPTION SPECIFICATIONS * * *
      *****************************************************************

     D CK1             S             70    DIM(5) CTDATA PERRCD(1)              COMMAND KEYS - SCR01
     D QCA             S             55    DIM(1) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC

     D DN              S             75    DIM(4)                               DOCN's
     D NOLNK           C                   CONST('No queues available ')
     D MDOC            C                   CONST('To Darts:')
     D MHAS            C                   CONST('has arrived on')
     D MQUE            C                   CONST('Queue:')
     D MSEN            C                   CONST('User :')
     D MID             C                   CONST('&QUEUE  ID')

     D DDAT            C                   CONST('Date:')
     D DTIM            C                   CONST('Time:')
     D DREA            C                   CONST('** INDEXED **')
     D DHBR            C                   CONST('has been reassigned')
     D DTOQ            C                   CONST('to Queue:')

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  YYDKEY                 1     40
     D  YYCO#                  1      3
     D  YYFND                  4      6
     D  YYEMP#                 7     15
     D  YYDIV                 16     20

      * * *  FILE INFORMATION DATA STRUCTURE
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     IPCM0I
     I              CMKEY                       PIKEY
     I              CMSEQ#                      PISEQ#
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************

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

     C                   MOVE      *BLANKS       DEKEY                          ADD 04/09/92

      *  EMPLOYER MASTER FILE

      *  EMPLOYER SEARCH
     C     ACDPL1        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM                    ##RRN2            9 0
     C                   PARM      'W'           ##QTE             1
     C                   PARM                    ##VIEW            1 0
     C                   PARM                    ##RRN3            9 0
      *
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXEMP#            9 0
     C                   KFLD                    XXDIV             5 0

      *   KEY LIST FOR THE COMPANY FILE

     C     COKEY         KLIST
     C                   KFLD                    XXCO#

      *   KEY LIST FOR THE FUND

     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND             3 0

      *  USER PARAMETER FILE

     C     UPKEY         KLIST
     C                   KFLD                    Q1USER

      *   KEY LIST FOR THE REMARKS DETAIL RECORD

     C     CMKYA         KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    ##SEQ#            5 0

      *   KEY LIST FOR THE REMARKS DETAIL RECORD

     C     CMKY          KLIST
     C                   KFLD                    QQKEY
      *
     C     CMKY1         KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    XXDATE
     C                   KFLD                    XXTIME
     C                   KFLD                    XXSEQ#
      *
     C     CMKY2         KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    @@CDAT            7 0

      *   KEY LIST FOR THE SECURITY FILE

     C     SLKY          KLIST
     C                   KFLD                    $$USER

      *   KEY LIST FOR THE SYSTEM CONTROL FILE

     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2

      *   KEY LIST FOR THE ADDITIONAL COMMENTS FILE

     C     ARKY          KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#

      *   KEY LIST FOR THE REMARK SUMMARY CODE FILE

      *   KEY LIST FOR THE REMARK TYPE FILE

     C     CTKY          KLIST
     C                   KFLD                    CMRMCD

     C     CTKY1         KLIST
     C                   KFLD                    SCCODE            5

     C     DTKY          KLIST
     C                   KFLD                    SCDOCT

     C     BMKY1         KLIST
     C                   KFLD                    QQBCHI

     C     DNKY2         KLIST
     C                   KFLD                    QQDCID

     C     RAKY          KLIST
     C                   KFLD                    QQDCID

      *   PARAMETER LIST FOR CALLED PROGRAM

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
     C                   PARM                    QQUNCN          255
     C                   PARM                    QQIFSP            1

     C                   CALL      'SA001R'
     C                   PARM      *BLANKS       ##WCIF            1            WC INST
     C                   PARM      *BLANKS       ##MCIF            1            MC INST
     C                   PARM      *BLANKS       ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANKS       ##AR              1            MC AR INST
     C                   PARM      *BLANKS       ##NTWK            1            NETWORK INST
     C                   PARM      *BLANKS       ##UR              1            PRECERT INST
     C                   PARM      *BLANKS       ##MED             1            MEDMGMT INST
     C                   PARM      *BLANKS       ##EDI             1            EDI INST
     C                   PARM      *BLANKS       ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANKS       ##RHAB            1            REHAB INST
     C                   PARM      *BLANKS       ##MSS             1            MS SUPPORT
     C                   PARM      *BLANKS       ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANKS       ##PRVB            1            PROVIDER BILLS
     C                   PARM      *BLANKS       ##NPWC            1            NON-PDS WC  LS
     C                   PARM      *BLANKS       ##OS              1            OUTOME/SAVINGS
     C                   PARM      *BLANKS       ##ARLS            1            Auto Release Flag
     C                   PARM      *BLANKS       ##CCOD            1            Correct Coding Flag
     C                   PARM      *BLANKS       ##CRVW            1            Code Review Flag
     C                   PARM      *BLANKS       ##PROM            1            ProMatch Flag
     C                   PARM      *BLANKS       ##MCRV            1            Medicare CR Flag
     C                   PARM      *BLANKS       ##FREQ            1            Frequency Flag
     C                   PARM      *BLANKS       ##CRAC            1            CR Auto Complete
     C                   PARM      *BLANKS       ##PREO            1            CR Auto Complete
     C                   PARM      *BLANKS       ##PARN            1            Par/Non-Par/Both
     C                   PARM      *BLANKS       ##MGRI            1            Multi Group Restr
     C                   PARM      *BLANKS       ##KCIF            1            Keep Claimant info
     C                   PARM      *BLANKS       ##CURS            1            PBILLS entry cursor
     C                   PARM      *BLANKS       ##PCIB            1            Prov Check in Batch
     C                   PARM      *BLANKS       ##CLT#            3            Client Number
     C                   PARM      *BLANKS       ##UCPF            1            UCR Percentile
     C                   PARM      *BLANKS       ##DCF             1            Data Center
     C                   PARM      *BLANKS       ##AUTB            1            Auto Batch   Flag
     C                   PARM      *BLANKS       ##DEIF            1            DE/Image Flag
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3

     C                   MOVE      *OFF          C114              1
     C     ##CLT#        IFEQ      '114'
     C                   MOVE      *ON           C114
     C                   ENDIF
     C*** Begin Add *** 04/10/19 **************************************
     C                   MOVE      *OFF          C152              1
     C     ##CLT#        IFEQ      '152'
     C                   MOVE      *ON           C152
     C                   ENDIF
     C*** End   Add *** 04/10/19 **************************************

      *** SAVE THE RTCD (F=FAX, I=IMG, " "= SCANNED IMAGE)
     C                   MOVEL     QQRTCD        QQDCT             1

     C     QQDCT         IFEQ      'F'
     C                   MOVE      *OFF          *IN73
     C                   ELSE
     C                   MOVE      *ON           *IN73
     C                   ENDIF
      /Free
       If *IN73 = *On;
          c1 = 5;
       Else;
          c1 = 1;
       Endif;
       av_keys = %subst(ck1(c1):1:55)  + '   ' + 'F10=Table File Search' +
        '   ' + %subst(ck1(2):20:35) + '   ' + %subst(ck1(3):1:35)   +
        '   ' + 'F23=Secure/Unsecure';
      /End-free

     C                   MOVEL     *BLANKS       QQRTCD

     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1

     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR

     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR

     C                   MOVEL     'SC'          XXSYK

      *** CONVERT SCAN DATE TO YYYYMMDD FOR USE WITH FILE OPS

      *** RESET POINT IN PROGRAM

     C     TOP           TAG
      *   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
      *   DISPLAY ALL FORMATS
      *** DISPLAY POINT
     C     STRT          TAG

     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END

      *******************************************************************
      *   PROCESS ALL COMMAND KEYS
      *******************************************************************

     C     *IN07         IFEQ      '1'
      ****************************************
      *   F3 = EXIT                          *
      ****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '3'           QQRTCD
     C                   MOVEL     '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
      ****************************************
      *   F5 = EMPLOYER - NUMBER SEARCH      *
      ****************************************
     C     *INKE         IFEQ      '1'

     C                   MOVE      '2'           ##MODE            1
     C                   CALL      'W0050R'      ACDPL1                 99
     C     ##RRN         CHAIN     PEM0P                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFND         SCFND
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   END
     C     *INKC         CABEQ     *ON           TERM

     C     *INKL         IFEQ      *ON
     C     SCEMP#        OREQ      *ZERO
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END

     C                   GOTO      STRT
     C                   END
      ****************************************
      *  F6 = BYPASS (ONLY FOR USER QUEUE)   *
      ****************************************
     C     *INKF         IFEQ      *ON
      *** BYPASS BY PUTTING A "P" IN THE RTNCD AND ENDING
     C                   MOVE      'P'           QQRTCD
     C                   GOTO      TERM

     C                   ENDIF                                                  END KF
      ****************************************
      *  F7 = Document Notes                *
      ****************************************
     C     *INKG         IFEQ      *ON

     C                   CALL      'IS016R'
     C                   PARM                    QQDCID
     C                   PARM      'ISII01R '    XXPNAM           10

     C                   ENDIF                                                  END KG
      ****************************************
      *  F8 = Reject Document               *
      ****************************************
     C     *INKH         IFEQ      *ON
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

     C     QQRTC         IFEQ      'A'
     C                   MOVE      '3'           QQRTCD
     C                   MOVEL     '1'           *INLR
     C                   GOTO      TERM
     C                   END

     C     QQRTC         IFEQ      'B'
     C                   GOTO      STRT
     C                   END
      ***** NOW VALID RETURN FROM REJECT, BYPASS THE CURRENT RCD.
     C                   MOVEL     '8'           QQRTCD                         REASS. RTN
     C                   GOTO      TERM
     C                   ENDIF                                                  END KH
      ****************************************
      *  F9 = ReASSIGN DOCUMENT             *
      ****************************************
     C     *INKI         IFEQ      *ON
      *** RE-ASSIGN ***
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

     C     @#RTC1        IFEQ      'A'
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF

     C     @#RTC1        IFEQ      'B'
     C                   MOVE      'C'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF

     C                   MOVEL     *BLANKS       QQKEY
     C                   MOVEL     '9'           QQRTCD                         REASS. RTN
     C                   GOTO      TERM
     C                   ENDIF                                                  END KH
      ****************************************
      *  F10 = TABLE FILE SEARCH             *
      ****************************************
     C     *INKJ         IFEQ      '1'
     C     SLF10         IFEQ      'Y'
      *  SCR01  *
     C     Q2RECF        IFEQ      'SCR01'
     C     FLD           IFEQ      'SCDUSR  '

     C                   CALL      'SC530R'
     C                   PARM                    SCDUSR           10
     C                   PARM                    QQRTCD            1

     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT

     C                   ENDIF

     C     SCDEPT        IFEQ      '? '
     C                   MOVEL     *BLANKS       SCCODE

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
     C                   MOVE      *BLANKS       SCCOMM
     C                   MOVEL     CTDESC        SCCOMM
     C                   END
     C                   GOTO      STRT
     C                   END                                                    SCCODE ='? '

     C                   MOVEL     SCPNAM        $$2A              2
     C     $$2A          IFEQ      '? '
     C                   MOVEL     *BLANKS       SCPNAM

     C                   MOVE      *BLANK        ZZRTCD
     C                   CALL      'SC590R'
     C                   PARM                    SCPNAM
     C                   PARM                    ZZRTCD
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    SCPNAM ='? '

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
     C                   MOVE      *BLANKS       SCCOMM
     C                   MOVEL     CTDESC        SCCOMM
     C                   END
     C                   GOTO      STRT
     C                   END                                                    SCCODE ='? '

     C                   END                                                    END SCR01

     C                   END                                                    END SLF10=Y
     C                   GOTO      STRT
     C                   END                                                    END *INKJ
      ****************************************
      *   F12 = PREVIOUS SCREEN(CANCEL)      *
      ****************************************
     C     *INKL         IFEQ      '1'
     C     SLF12         IFEQ      'Y'
      *  SCR01  *
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      'C'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END

     C                   END                                                    END SLF12=Y
     C                   GOTO      STRT
     C                   END                                                    END *INKL
      *****************************************************************
      ** INKM - FUNCTION 13 - CHANGE USER PARMS
      *****************************************************************

      *   PROCESS CHANGE OF USER PARAMETERS

     C     *INKM         IFEQ      '1'

     C                   CALL      'W0006R'
     C                   WRITE     SCR99
      *
      *  GET THE USER PARAMETER FILE
      *
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
      *****************************************************************
      ** INKN - FUNCTION 14 - SPLIT REQUEST
      *****************************************************************
      *
      *   PROCESS SPLIT REQUEST
      *
     C     *INKN         IFEQ      '1'
      *
      *
     C                   MOVE      QQSDAT        #$SDT             8
     C                   MOVE      QQSTIM        #$STM             6
      *
     C     QQDCT         IFEQ      'F'
     C                   MOVEL     'F'           Q@RTC             1
     C                   ELSE
     C                   MOVEL     'I'           Q@RTC
     C                   ENDIF
      *
     C                   MOVEL     'I'           Q@RTX
     C                   CALL      'IS215R3'
     C                   PARM      QQQID         $$QID            10
     C                   PARM      QQDCID        $$DCID           12
     C                   PARM                    #$SDT             8
     C                   PARM                    #$STM             6
     C                   PARM      QQDOCT        $$DOCT            8
     C                   PARM                    Q@RTX             1             FRM INDX PG
      *
     C     Q@RTX         IFEQ      '3'
     C                   MOVEL     Q@RTX         QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
      *
      *
     C     Q@RTX         IFEQ      '*'
     C                   ENDIF
      *
      *
     C     Q@RTX         CABEQ     'C'           STRT                             F12
      *
     C                   GOTO      STRT
      *
     C                   END
      *
      *
      *****************************************************************
      ** INKQ - FUNCTION 16 - NEW INDEX PROGRAM
      *****************************************************************
      *
      *   PROCESS INDEX CHANGE
      *
     C     *INKQ         IFEQ      '1'
      *
     C                   MOVEL     'K'           QQRTCD
      *
     C                   GOTO      TERM
     C                   END
      *
      ****************************************
      *  F17 = USER PROFILE SEARCH           *
      ****************************************
     C     *INKR         IFEQ      '1'
     C     SLF09         IFEQ      'Y'
      *
     C     Q2RECF        IFEQ      'SCR01'
      *
     C                   CALL      'SC530R'
     C                   PARM                    SCDUSR           10
     C                   PARM                    ZZRTCD            1
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END SCR01
      *
     C                   END                                                    END SLF09=Y
     C                   GOTO      STRT
     C                   END                                                    END *INKI
      ****************************************
      *   F23=SECURE REMARKS  BY LEVEL       *
      ****************************************
     C     *INKX         IFEQ      '1'
     C     SLF23         IFEQ      'Y'
      ***********
      *  SCR01  *
      ***********
      *  Remark UNSECURED to SECURED
     C     Q2RECF        IFEQ      'SCR01'
     C     *IN56         IFEQ      '0'
     C                   MOVEL     *BLANKS       SCCODE
     C                   MOVEL     SCDEPT        SCCODE
     C                   MOVE      SCRMTY        SCCODE
     C     CTKY1         CHAIN     PCT01                              59        +DEFAULT SECLVL
     C     *IN59         IFEQ      '1'
     C                   MOVE      SLSECL        SCSECL
     C                   ELSE
     C                   END
     C                   MOVE      '1'           *IN56
     C                   MOVE      SCSECL        ##SECL            3 0
     C                   MOVE      'Y'           ##SBYL            1
      *** DISPLAY "UNSECURE REMARKS", IF "SECURE REMARKS" WAS DISPLAYED
     C     C1            IFEQ      3
     C                   Z-ADD     4             C1
     C                   END
      *
     C                   ELSE
      *  Remark SECURED to UNSECURED
     C                   MOVE      '0'           *IN56
     C                   MOVE      *ZEROS        SCSECL
     C                   MOVE      SCSECL        ##SECL            3 0
     C                   MOVE      'N'           ##SBYL            1
      *** DISPLAY "SECURE REMARKS", IF "UNSECURE REMARKS" WAS DISPLAYED
     C     C1            IFEQ      4
     C                   Z-ADD     3             C1
     C                   END
     C                   END
     C                   MOVEL     CK1(C1)       SCCML1                         ADD 04/06/92
      *
     C                   END
      *
     C                   END                                                    END SLF23=Y
      *
     C                   GOTO      STRT
     C                   END                                                    END *INKX
      ****************************************
      *   F24=More Keys                      *
      ****************************************
     C     *INKY         IFEQ      '1'
     C     SLF24         IFEQ      'Y'
      ***********
      *  SCR01  *
      ***********
     C     Q2RECF        IFEQ      'SCR01'
      *
     C     C1            IFEQ      1
     C     C1            OREQ      5
     C                   Z-ADD     2             C1
     C                   GOTO      CMDK#
     C                   END
      *  Remark Secured
     C     *IN56         IFEQ      '1'
     C     C1            ANDEQ     2
     C                   Z-ADD     4             C1
     C                   GOTO      CMDK#
     C                   END
      *  Remark NOT Secured
     C     *IN56         IFEQ      '0'
     C     C1            ANDEQ     2
     C                   Z-ADD     3             C1
     C                   GOTO      CMDK#
     C                   END

     C     C1            IFEQ      3
     C     C1            OREQ      4

     C     *IN73         IFEQ      *ON
     C                   Z-ADD     5             C1
     C                   ELSE
     C                   Z-ADD     1             C1
     C                   ENDIF
     C                   GOTO      CMDK#
     C                   END
      *
     C     CMDK#         TAG
      *
     C                   MOVEL     CK1(C1)       SCCML1
      *
     C                   END                                                    END SCR01
     C                   END                                                    END S24
     C                   GOTO      STRT
     C                   END                                                    END *INKY
      *
     C                   GOTO      STRT
     C                   END                                                    *IN11/CMDKEYS
      *****************************************************************
      *   GOTO THE APPROPRIATE SCREEN
      *****************************************************************
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
      *
      *   PROCESS REQUESTED SCREEN
      *
      *------------------------*
     C     SCR01T        TAG
      *
     C                   MOVE      SCCO#         ##CO#             3
     C                   MOVE      SCFND         ##FND             3
     C                   MOVE      SCEMP#        ##EMP#            9
     C                   MOVEL     SCDIV         ##DIV             5
      *
     C     ##CO#         CAT(P)    ##FND:0       SCKEY            40
     C                   CAT(P)    ##EMP#:0      SCKEY
     C                   CAT(P)    ##DIV:0       SCKEY
      *
     C                   EXSR      EDTS01                                        EDIT SCR01 FIELDS
      *
      *** WRITE REMARK RECORD
      *
      *  *** MOVE SCREEN KEY INTO CM RCD
     C                   MOVE      SCCO#         ##CO#             3
     C                   MOVE      SCFND         ##FND             3
     C                   MOVE      SCEMP#        ##EMP#            9
     C                   MOVEL     SCDIV         ##DIV             5
      *
     C     ##CO#         CAT(P)    ##FND:0       ##KEY            40
     C                   CAT(P)    ##EMP#:0      ##KEY
     C                   CAT(P)    ##DIV:0       ##KEY
      *
      *  *** PROGRAM NAME TO USE FOR PNAM - KEY STRUCTURE ?
     C                   MOVEL     'W0050R  '    ##PNAM           10
      *
     C                   EXSR      WCOM
      *
      * * * EXSR HERE TO WRITE DOC NOTES, AND ALSO TO COPY ALL DOC NOTE
      * * *  TO THE SMARP FILE FOR THIS NEW DART REMARK
      *
     C                   EXSR      DNOT
      *
      *
      *** RETAIN THE ITEM ON THE USERS QUEUE?
     C     SCRETN        IFEQ      'Y'
     C                   MOVEL     'R'           QQRTCD
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01 '      Q2RECF
      *------------------------*
     C     TERM          TAG
      *------------------------*
      *
     C                   MOVE      '1'           *INLR
      *====================================================================*
      * * *    NIT SUBROUTINE                                    I N I T ***
      *====================================================================*
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
      *** GET USER PARM
     C     UPKEY         CHAIN     PUP01                              60
     C                   MOVE      *ZERO         SCEMP#
     C                   MOVE      *ZERO         SCDIV
     C                   MOVE      UPCO#         XXCO#             3 0
     C                   MOVE      UPFND         XXFND             3 0
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         XXCO#
     C                   MOVE      *ZERO         XXFND
     C                   MOVE      *ZEROS        SCCO#
     C                   MOVE      *ZEROS        SCFND
     C                   END
      *  GET THE COMPANY AND FUND RECORDS
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CONAME        $$CNAM           40
     C                   MOVE      XXCO#         SCCO#
     C                   ELSE
     C                   MOVEL     *BLANKS       $$CNAM
     C                   MOVE      *ZEROS        SCCO#
     C                   ENDIF
      *
      * TRY TO RETRIEVE DATA FROM IMRAP
      *
     C     RAKY          CHAIN     PRA01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RADKEY        YYDKEY
     C                   MOVE      YYCO#         XXCO#
     C                   MOVE      YYFND         XXFND
     C                   MOVE      YYCO#         SCCO#
     C                   MOVE      YYFND         SCFND
     C                   MOVE      YYEMP#        SCEMP#
     C                   MOVE      YYDIV         SCDIV
     C                   ENDIF
      *
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      XXFND         SCFND
     C                   MOVEL     FNNAME        $$FNAM           40
     C                   ELSE
     C                   MOVE      *ZERO         SCFND
     C                   MOVE      *BLANKS       $$FNAM
     C                   END
      *
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
      *
     C                   MOVEL     SLFOLD        SCDFLR           80
      *
     C     SLCCIF        IFNE      'Y'
     C                   MOVE      '1'           *IN98
     C                   ELSE
     C                   MOVE      '0'           *IN98
     C                   END
      *
      *** Check System Control File Install Flags for this Client:
     C     'SY'          CHAIN     SSY01                              60
     C     *IN60         IFEQ      '0'
      *
      *   Check for AS/400 Office Installed
     C     SYOFIF        IFEQ      'Y'
     C                   MOVE      'Y'           XXOFIF            1            Installed
     C                   MOVE      '0'           *IN03                          ADD 04/06/92
     C                   ELSE                                                   ELS 'Y' SYOFIF
     C                   MOVE      'N'           XXOFIF                         Installed
     C                   MOVE      '1'           *IN03                          ADD 04/06/92
     C                   END                                                    END 'Y' SYOFIF
      *
     C                   END                                                    END 0 *IN60
      *
      * LOOK AT DARTS SYSTEM CONTROL FILE TO DETERMINE WHAT'S INSTALLED
     C     SYKY          CHAIN     SDR01                              60
     C     *IN60         IFEQ      '0'
      *
      *** IF IMAGE IS INSTALLED, SET ON IMAGE INDICATOR.
     C     DRFL01        IFEQ      'Y'
     C                   MOVE      '1'           *IN99
     C                   END
      *
      *      (DRFL03 = 'Y', *IN34 = 1)
     C     DRFL03        IFEQ      'Y'                                          ADD 04/09/92
     C                   MOVE      '1'           *IN34
     C                   END                                                    ADD 04/09/92
      *
      *** IF SUMMARY CODE  OPTION IS INSTALLED, SET ON INDICATOR.
      *      (DRFL02 = 'Y', *IN36 = 1), INITIALIZE        TO *BLANKS
     C     DRFL02        IFEQ      'Y'                                          ADD 04/21/92
     C                   MOVE      '1'           *IN36                          ADD 04/21/92
     C                   END                                                    ADD 04/21/92
      *
     C                   END                                                    END 0 *IN60
      *
     C     *IN73         IFEQ      *ON
     C                   Z-ADD     5             C1
     C                   ELSE
     C                   Z-ADD     1             C1
     C                   END                                                    END DCT = F
      *
     C                   MOVEL     CK1(C1)       SCCML1
      *   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
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
      *
     C                   MOVEL     'N'           SCARMK
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        SCCDAT
      *
     C     DTKY          CHAIN     PDT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     DTRMCD        SCDEPT
     C                   MOVE      DTRMCD        SCRMTY
     C                   ELSE
     C                   MOVEL     *BLANKS       SCDEPT
     C                   MOVE      *BLANKS       SCRMTY
     C                   ENDIF
      *
      * FILL IN THE DEFAULT DIARY TYPE HERE
     C                   MOVEL     SCDEPT        SCCODE
     C                   MOVE      SCRMTY        SCCODE
     C     CTKY1         CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CTFL01        SCDITP
     C                   ELSE
     C                   MOVEL     *BLANKS       SCDITP
     C                   ENDIF                                                  *IN60  = *OFF
      *
      * DEFAULT THE DIARY USER TO CURRENT USER IF THE DIARY FLAG IS Y
      *
     C     SCDITP        IFEQ      'Y'
     C                   MOVEL     Q1USER        SCDUSR
     C                   ENDIF
      *
     C     BMKY1         CHAIN     PBM01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     BMBDSC        SCBDSC           40
     C                   ELSE
     C                   MOVEL     *BLANKS       SCBDSC
     C                   ENDIF
      *
     C                   MOVEL     SCBDSC        SCCOMM
      *
      ** ** ANY NON-SYSTEM ** **  DOC NOTES EXIST????
     C     DNKY2         CHAIN     PDN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN69
     C                   ELSE
     C                   MOVE      *OFF          *IN69
     C                   ENDIF
      *
     C                   ENDSR
      *
      *====================================================================*
      * * *   WCOM  SUBROUTINE - WRITE REMARK RECORD           W C O M   ***
      *====================================================================*
      *
     C     WCOM          BEGSR
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT              7 0
     C                   Z-ADD     DSTIME        HHTIME            6 0
      *
      *** GET LAST SEQ# USED
      *
      * * * WRITE REMARKS  TO FILE * * *
      *
     C                   MOVE      ##SECL        CMSECL
     C                   MOVE      ##SBYL        CMSBYL
      *
     C                   MOVEL     ##KEY         CMKEY
     C                   MOVEL     *BLANKS       CMCSID                         ANY LONGER
     C                   MOVEL     ##PNAM        CMPNAM
     C                   MOVE      *BLANKS       CMCODE
     C                   MOVEL     SCCODE        CMRMCD
     C                   MOVE      ' '           CMFL01
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      CTSMCD        CMSMCD
     C     ##DEIF        IFEQ      'Y'
     C     CTDEPG        ANDNE     *BLANKS
     C                   MOVE      'Y'           CMFL01
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *BLANKS       CMSMCD
     C                   MOVE      *BLANKS       CTDEPG
     C                   ENDIF
     C                   MOVEL     '***IMAGE'    CMDCID
     C                   MOVE      '*** '        CMDCID
     C                   MOVEL     SCCOMM        CMCOMM
      ****   LOAD FAX FOLDER HERE *********************
     C                   MOVEL     QQPTH         CMFOLD
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMCDAT
     C                   Z-ADD     HHDT          CMTRDT
     C                   Z-ADD     HHTIME        CMTRTM
     C                   MOVEL     QQUNCN        CMUNCN
     C                   MOVEL     QQIFSP        CMIFSP
      ***** FILL HIDDEN FIELDS ****
      *
      * * * IMAGE FIELDS
     C                   MOVEL     SCIQUE        CMIQUE                             HIDDEN
     C                   MOVEL     SCIPTH        CMIPTH                             HIDDEN
     C                   MOVE      SCPIDX        CMPIDX                             HIDDEN
     C                   MOVEL     SCAPID        CMAPID                             HIDDEN
     C                   MOVEL     QQDCID        CMIDOC                             HIDDEN
     C                   MOVEL     SCQPTY        CMQPTY                             HIDDEN
     C                   MOVEL     SCDCTP        CMDCTP                             HIDDEN
      *** FAX FLAG
     C     QQDCT         IFNE      'F'
     C                   MOVEL     'Y'           CMIMGF                             HIDDEN
     C                   ELSE
     C                   MOVEL     'F'           CMIMGF
     C                   MOVEL     QQDCID        CMDCID
     C                   MOVE      *BLANKS       CMIDOC
     C                   ENDIF
      *
     C                   MOVEL     *BLANK        CMSTAT
     C                   MOVEL     Q1PGM         CMPGM
      *** IF DIARY USER ENTERED, WRITE TO CMUSER
     C     SCDUSR        IFNE      *BLANKS
     C                   MOVEL     SCDUSR        CMUSER
     C                   ELSE                                                   ELSE NE *BLANKS
      *** ELSE DEFAULT TO CURRENT USER
     C                   MOVEL     Q1USER        CMUSER
     C                   END                                                    END NE *BLANKS
      *
     C                   MOVEL     Q1USER        CMIUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C     SCDITP        IFEQ      'Y'
     C     SCDITP        OREQ      'N'
      *
     C                   MOVE      SCDITP        CMDIFL
     C                   ELSE
     C                   MOVE      ' '           CMDIFL
     C                   END
      *                                                    DFT LOC=DASD
     C                   MOVEL     'D'           CMFL05
      *
     C                   MOVE      SCPGFL        CMFL02
     C                   Z-ADD     DSDATE        SRDTI                          Force this a
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMIDT
      *
     C                   Z-ADD     *ZERO         CMDCDT
     C                   MOVEL     Q1PGM         CMIPGM
      *
     C                   MOVE      *BLANKS       CMFL04
     C                   MOVE      *BLANKS       CMFL05
      *
      *** GET LAST SEQ# USED
      *
     C                   MOVEL     SCKEY         QQKEY            40
     C                   EVAL      *IN99=*OFF
     C                   DOU       NOT *IN99
      *
     C     CMKYA         SETGT     PCM0I
     C                   READP     PCM0I                                  60
     C     *IN60         IFEQ      '0'
     C     QQKEY         ANDEQ     PIKEY
     C     PISEQ#        ADD       1             CMSEQ#
     C                   ELSE
     C                   Z-ADD     0             CMSEQ#
     C                   END
      *
     C                   WRITE     PCM01                                99
     C                   ENDDO
      *
     C                   FEOD      SDCML
     C     ##DEIF        IFEQ      'Y'
     C                   EXSR      WRTAIP
     C                   ENDIF
     C                   MOVEL     *BLANKS       QQRTCD
      *
     C                   MOVE      CMSEQ#        QQSEQ#
      *
      *** IF ADDITIONAL RMRKS ARE REQUESTED (SCARMK='Y') THEN CALL PROGRAM T
      *** TO ADD THE REMARKS.
      *
     C                   MOVEL     'ISII01R '    QQPNAM
     C     SCARMK        IFEQ      'Y'
     C                   CALL      'SC014R'
     C                   PARM                    QQKEY
     C                   PARM                    CMSEQ#
     C                   PARM                    QQPNAM           10
     C                   END
      *
     C                   ENDSR
      *====================================================================*
      * * *   EDTS01  SUBROUTINE - EDIT SCREEN 1 FIELDS    E D T S 0 1   ***
      *====================================================================*
     C     EDTS01        BEGSR
      * VALIDATE EMPLOYER AND DIVISION
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
     C     EMKEY         CHAIN     PEM01                              21
     C     *IN21         CABEQ     '1'           STRT
      *** MUST ENTER VALID DATE ***
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCDAT
      *
     C     SRERR         CABEQ     '1'           STRT                     52
      *
     C                   MOVE      ' '           #DIARY            1             DIARY TYPE FLAG
      *   REMARK TYPE MUST BE ENTERED, AND IT MUST BE VALID
     C                   MOVEL     SCDEPT        SCCODE
     C                   MOVE      SCRMTY        SCCODE
      *    If remark is secured, the secuity level displayed on screen
      *      is moved into the field to be stored when record is made
      *      or updated.
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
      *
      **** DETERMINE IF REMARK TYPE IS DIARY OR NOT *****
      ***                  ELSE
      ***********          END
      *
     C                   END                                                     SCCODE NE *BLANKS
      *
     C     SCDITP        IFEQ      'Y'
      *
      *** MUST ENTER A VALID USER NAME
      *
     C                   MOVEL     SCDUSR        $$USER
     C     SLKY          CHAIN     PSL01                              58
     C     *IN58         CABEQ     '1'           STRT
     C                   END
      *
     C     SCDITP        IFNE      ' '
     C     SCDITP        IFNE      'Y'
     C     SCDITP        ANDNE     'N'
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   END                                                    END SCDITP NE Y/N
     C                   ELSE                                                   ELSE SCDITP NE ' '
     C                   MOVE      CTFL01        SCDITP
     C                   END                                                    END SCDITP NE ' '
      *
      *
      *  Check Data Entry Flag to see if Data Entry Program has been
      *     called once already.
      *
     C     XXDEFL        IFEQ      'N'
      *  If DRFL03 = 'Y' in file SDRPF, Remark Code is checked to see
      *     if it has an associated Data Entry Program ID.
      *
     C                   MOVE      'N'           XXDEPG            1
     C     SYKY          CHAIN     SDR01                              40
     C     *IN40         IFEQ      '0'
     C     DRFL03        IFEQ      'Y'
     C     CTDEPG        IFNE      *BLANKS
     C                   MOVE      'Y'           XXDEPG
      *
     C     SCDCID        IFNE      *BLANKS
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
      *  Check to see if a recursive call was made.  If so... error msg
      *
     C                   MOVE      *BLANKS       XXPGM            10
     C                   MOVEL     CTDEPG        XXPGM
     C                   MOVE      *BLANKS       SCDCID
     C                   MOVEL     CTDEPG        SCDCID
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDSDT
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
     C     XXCDAT        IFLE      XXDSDT
      *
     C     ERRFLE        IFEQ      ' '
     C                   CALL      'ERROR1C'
      *
     C                   Z-ADD     55            LENGTH           15 5
     C                   CALL      'QCMDEXC'                              37
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH
      *
     C                   OPEN      WXERP1
     C                   MOVE      'Y'           ERRFLE            1
     C                   END
      *
     C     7             CHAIN     PER01                              37
     C     *IN37         DOWEQ     '0'
      *  Check for Recursive call to program
     C     ERPGM         IFEQ      XXPGM
     C                   WRITE     SCR99
     C                   WRITE     SCR01
     C                   MOVE      '1'           *IN47
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
      *
     C                   READ      PER01                                  37
     C                   END
      *
      *  ALL EDITS PASSED - CALL DATA ENTRY PROGRAM
      *  (NOTE: It was necessary to break up the comment field of size
      *   275 into DECOM1 and DECOM2 because the largest alpha-numeric
      *   value allowed for a parameter is 256.
      *
     C                   MOVE      *BLANKS       DEKEY
     C                   MOVE      *BLANKS       DECOM1
     C                   MOVE      *BLANKS       DECOM2
     C                   MOVE      *BLANKS       DERTCD
     C                   MOVEL     QQKEY         DEKEY
      *
     C                   CALL      CTDEPG                               37
     C                   PARM                    DEKEY
     C                   PARM                    DECOM1
     C                   PARM                    DECOM2
     C                   PARM                    DERTCD
      *
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
      *
     C     DERTCD        IFEQ      'A'
     C     DERTCD        OREQ      'B'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
      *
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
      *
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
      *
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   END
      *
     C                   END                                                    CTDEPG NE 0
     C                   END                                                    DRFL03 = 'Y'
     C                   END                                                    *IN40 = 0
     C                   END
      *
     C     *IN99         IFEQ      '0'                                           NO EDIT IF IMAGE
     C     XXDEFL        ANDEQ     'N'
      *
      *** IF ENTERED, DOC ID MUST BE VALID (EXISTING)
      *
     C                   MOVEL     SCDCID        XXDCID
     C                   MOVEL     SCDFLR        XXFOLD           80
     C                   MOVEL     *BLANK        ZZRTCD            1
      *
     C     SCDCID        IFNE      *BLANK
      *
      *** If AS/400 Office NOT Installed - Don't allow this function
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
      *
      *
      *** Must have a Default Folder entered for this function
     C     XXFOLD        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
      *
     C                   CALL      'SVALDOC'
     C                   PARM                    XXDCID           12
     C                   PARM                    XXFOLD
     C                   PARM                    ZZRTCD            1
      *
     C     ZZRTCD        CABEQ     '1'           STRT                     54
      *
     C                   END                                                    DCID <> *BLANK
      *
     C                   END                                                    99 = 0
      *
     C     SCCOMM        CABEQ     *BLANKS       STRT                     53
      *
      *** MUST ENTER VALID ADDITIONAL REMARKS FLAG
     C     SCARMK        IFNE      'Y'
     C     SCARMK        CABNE     'N'           STRT                 4646
     C                   END
      *
      * PURGEABLE ITEM FLAG MUST BE 'Y', 'N', OR BLANK
      *
     C     SCPGFL        IFNE      'Y'
     C     SCPGFL        ANDNE     'N'
     C     SCPGFL        ANDNE     *BLANK
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
      *
      * IF PURGEABLE ITEM FLAG IS = 'Y'
      *
     C     SCPGFL        IFEQ      'Y'
      *
     C     CTDPRG        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN43
     C                   GOTO      STRT
     C                   END
      *
     C     SCDITP        IFNE      'Y'
     C                   MOVE      '1'           *IN44
     C                   GOTO      STRT
     C                   END
      *
     C                   END
      *
      * IF PURGEABLE ITEM FLAG IS = *BLANKS
      *
     C     SCPGFL        IFEQ      *BLANKS
      *
     C     CTDPRG        IFNE      *ZEROS
     C     SCDITP        ANDEQ     'Y'
     C                   MOVE      'Y'           SCPGFL
     C                   ELSE
     C                   MOVE      'N'           SCPGFL
     C                   END
      *
     C                   END
      * Verify is correct Employer
     C     C114          IFEQ      *ON
     C*** Begin Add *** 04/10/19 **************************************
     C     C152          OREQ      *ON
     C*** End   Add *** 04/10/19 **************************************
     C                   MOVEL(P)  SCKEY         SCPKEY
     C                   MOVEL(P)  EMNAME        SCENME
     C                   EXFMT     ISIIVLF
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C     *INKL         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL = *ON
     C                   ENDIF
      *
     C                   ENDSR
      *====================================================================*
      * * *   ERROR SUBROUTINE - EXCEPTION/ERROR SUBROUTINE           * * **
      *====================================================================*
     C     ERROR         BEGSR
      *
      *  IF FILE IS NOT FOUND IN USERS LIBL
      *
     C     Q1STCD        IFEQ      211
      *
     C                   END
      *
     C                   ENDSR
      *====================================================================*
      * * *   DNOT DOC NOTES  - CREATE DOC NOTES AND CPY NOTES TO SMAR* * **
      *====================================================================*
     C     DNOT          BEGSR
      *
      *** FILL ALL DOC NOTE FIELDS
      ** GET CURRENT DATE/TIME TO CREATE NOTES...
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
      *
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
      *
     C                   MOVEL     XX75          DN(1)
      *
     C                   MOVE      CMSEQ#        $XSEQ             5
     C     MDOC          CAT(P)    CMKEY:1       XX75
     C                   CAT(P)    $XSEQ:5       XX75
      *
     C                   MOVEL     XX75          DN(2)
      *
     C     MSEN          CAT(P)    Q1USER:1      XX75
     C                   CAT(P)    '.'           XX75
     C                   MOVEL     XX75          DN(3)
     C                   MOVEL     *BLANKS       DN(4)
      *
      *
      *** CALL PGM TO WRITE DOC NOTES
      *
      * * * DOW LOOP HERE TO PROCESS MULTIPLE ITEMS.  SETUP ITEMS
      * * * AS COMMENTS, OR ARRAY ELEMENTS
      *
      *
     C                   DO        4             G                 3 0
      *
     C                   MOVEL     QQDCID        $#DCID           12
     C                   MOVEL     DN(G)         QQCM75
      *
     C     G             IFGE      4
     C                   MOVE      '1'           Q$RTC             1
     C                   ELSE
     C                   MOVE      ' '           Q$RTC
     C                   ENDIF
      *
     C                   CALL      'ISDOCNR'
     C                   PARM                    QQCM75           75              COMMENT
     C                   PARM                    QQDCID           12              DOCID
     C                   PARM                    #$NOWD                           NOTE DATE
     C                   PARM      Q1PGM         QQPNAM           10              CALLING PGM
     C                   PARM      'H'           QQPRFL            1              PROTECT FLG
     C                   PARM                    Q$RTC             1              RTN CODE
      *
     C                   ENDDO
      *
      * * * Now we need to create the Additional Remarks for this
      * * *  DART, by read/writing all Notes to the SMARP file.
      * * *  We will also BACK DATE the Remarks, so they are NOT modifi
      * * *  able.
      *
     C                   CALL      'ISNOTAR'
     C                   PARM                    QQDCID           12
     C                   PARM      CMKEY         Q@KEY            40
     C                   PARM      CMSEQ#        Q@SEQ#            5 0
     C                   PARM      *BLANKS       Q@RCD             1
      *
     C                   ENDSR
      *** BEGIN ADD *** 06/14/04 **************************************
      *====================================================================*
      * WRTAIP - WRITE SDAIP                                            * **
      *====================================================================*
     C     WRTAIP        BEGSR
      *
     C                   CLEAR                   PAI01
      *
     C                   MOVEL(P)  CMKEY         AIKEY
     C                   Z-ADD     CMSEQ#        AISEQ#
      *
     C                   MOVEL(P)  CTDEPG        AIDPGM
      *
     C     AIDPGM        IFNE      *BLANKS
     C                   MOVEL(P)  CMKEY         AIDKEY
     C                   ENDIF
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVEL(P)  Q1USER        AIUSER
     C                   MOVEL(P)  Q1PGM         AIPGM
     C                   Z-ADD     DSTIME        AITIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AIDT
      *
     C                   MOVEL(P)  Q1USER        AIIUSR
     C                   MOVEL(P)  Q1PGM         AIIPGM
     C                   Z-ADD     DSTIME        AIITIM
     C                   Z-ADD     AIDT          AIIDT
      *
     C                   WRITE     PAI01
      *
     C                   ENDSR
**  CK1 - Command Keys - SCR01
F3=Exit  F5=Search  F7=Notes   F8=Reject   F9=Reassign   F24=More keys
F10=Search         F12=Previous   F13=Chg User Parms     F24=More keys
F14=Split    F16=New Index Type    F23=Secure Remarks    F24=More keys
F14=Split    F16=New Index Type    F23=Unsecure Remarks  F24=More keys
F3=Exit  F5=Search  F6=Bypass   F7=Notes   F9=Reassign   F24=More keys
**  QCMDEXC TABLE
OVRDBF FILE(WXERP1) TOFILE(QTEMP/WXERP1 )
