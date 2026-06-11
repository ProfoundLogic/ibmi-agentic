     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H****************************************************************
     H/TITLE F0006R - USER PARAMETER FILE MAINTENANCE
     H****************************************************************
     H* REVISIONS:                                                   *
     H*                                                              *
     H* 12/18/24 C:342 Project : MC447 Phase 2                       *
     H*                1.F10 Search disabled on the Group Field      *
     H*                DSPF Changes:                                 *
     H*                1.Changed Existing Group field into Typeahead *
     H*                                                              *
     H* 03/01/16  C:65  Project: SY251                               *
     H*                 Add network printer dropdown for selection   *
     H*                 of default printer for scriptura enabled     *
     H*                 processes.                                   *
     H*
     H*  04/30/13 C:277 PROJECT 147MAS
     H*                 Change screen into Transform & Recompile
     H*
     H*  10/28/02 C:62 PROJECT MC253
     H*                MULTI GROUP RESTRICTIONS LOGIC
     H*
     H*  01/02/01 C:62 PROJECT:MCMNT   BASE ONLY
     H*                ALLOW F12=CANCEL.
     H*
     H*  05/16/00 C:33 SEQ#: OP082/21740
     H*                IF DIV ARE RESTRICTED DO NOT ALLOW ACCESS.
     H*
     H*  06/30/99 C:62 PROJECT:MC206
     H*                CHECK TO SEE IF CALLED FROM F0060R AND MAKE
     H*                DUPLICATE PGM TO CALL FOR GROUP SEARCH.
     H*
     H*  06/07/99 C:53 MC145
     H*                REMOVE WMUP* AND ADD A CALL TO UPDATE THE FILE
     H*                IF PDS WORKERS COMP IS INSTALLED.
     H*
     H*  03/27/99 C:33 PROJECT: MCMNT
     H*                AVOID RECURRSIVE CALLS IN SEARCES.
     H*
     H*  03/17/99 C:61   PROJECT: MCMNT
     H*                - Do not allow search of protected fields.
     H*
     H*  10/15/98 C:110  REQ/PROJ: MC145
     H*                  1. CHANGED F10 COMPANY SEARCH (MOVEL)
     H*
     H*  04/29/98 C:53   REQ/PROJ: MC145
     H*                  UC WORKER COMP FILE
     H*
     H*  02/26/98 C:62   REQ/PROJ: MC171
     H*                  1. ADDED EMPLOYER/DIVISION.
     H*                  2. ADDED LOGIC FOR RESTRICTION FLAGS.
     H*                  3. ADDED NAME FIELDS FOR CO#/SUB#/GRP/EMP#.
     H*                  4. CHANGED ALL SEARCHES TO F10.
     H*
     H*  09/27/93 C:20   REQ/PROJ: SY007
     H*                  1. ADDED F6=CO# SEARCH
     H*                  2. ADDED F7=SUB ALPHA SEARCH
     H*                  3. ADDED F17=SUB# SEARCH
     H*                  4. ADDED F8=GROUP ALPHA SEARCH
     H*                  5. ADDED F18=GROUP # SEARCH
     H*
     H*  07/21/92 KRR    PROJ: 99AAC
     H*                  1. ADDED SUB# TO ALL PROCESSING
     H*
     H*              LJQ  8-23-91   ADDED WORK COMP USER PARM FILE.
     H*                             FILE NEEDS TO BE UPDATED WHEN
     H*                             SYSTEM USER PARM IS UPDATED
     H*
     H****************************************************************
     H*
     FF0006F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMUPL     UF A E           K DISK
     FSMCOL     IF   E           K DISK
     FFMSMP     IF   E             DISK
     FFMSML     IF   E           K DISK
     F                                     RENAME(PSM01:PSM02)
     F                                     INFDS(DBFDS)
     FFMFNP     IF   E             DISK
     FFMFNL     IF   E           K DISK
     F                                     RENAME(PFN01:PFN02)
     F                                     INFDS(DBFDS2)
     FFMEMP     IF   E             DISK
     FFMEML     IF   E           K DISK
     F                                     RENAME(PEM01:PEM02)

     D UN              S             40    DIM(1) CTDATA PERRCD(1)
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
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
     C     UPKEY         KLIST
     C                   KFLD                    XXUSR            10
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*   KEY LIST FOR THE SUBSCRIBER
     C*
     C     SMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C*
     C*   KEY LIST FOR THE FUND FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C*
     C     FNKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C*   KEY LIST FOR THE EMPLOYER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     *LIKE         DEFINE    EMCO#         KKCO#
     C     *LIKE         DEFINE    EMCO#         QQCO#
     C     *LIKE         DEFINE    EMSUB#        KKSUB#
     C     *LIKE         DEFINE    EMFND         KKFND
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C     *LIKE         DEFINE    EMDIV         KKDIV
     C     *LIKE         DEFINE    UPFND         ##FND
     C     *LIKE         DEFINE    UPEMP#        ##EMP#
     C     *LIKE         DEFINE    UPSUB#        ##SUB#
     C*
     C                   TIME                    DSDTTM
     C***  begin add  ***  03/01/16  **************************************
     C                   Call      'SA047R'
     C                   Parm                    ##Script          1
     C                   If        ##Script = 'Y'
     C                   Eval      dspScript = *on
     C                   Else
     C                   Eval      dspScript = *off
     C                   Endif
     C***  end add ******  03/01/16  **************************************
     C                   MOVE      UN(1)         SCPRDS
     C                   MOVE      UN(1)         QQPRDS           40
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
      * CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
      *
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
     C                   PARM                    ##OS              1            OUTOME/SAVINGS
     C                   PARM                    ##ARLS            1            Auto Release Flag
     C                   PARM                    ##CCOD            1            Correct Coding Flag
     C                   PARM                    ##CRVW            1            Code Review Flag
     C                   PARM                    ##PROM            1            ProMatch Flag
     C                   PARM                    ##MCRV            1            Medicare CR Flag
     C                   PARM                    ##FREQ            1            Frequency Flag
     C                   PARM                    ##CRAC            1            CR Auto Complete
     C                   PARM                    ##PREO            1            CR Auto Complete
     C                   PARM                    ##PARN            1            Par/Non-Par/Both
     C                   PARM                    ##MGRI            1            Multi Group Restr
     C*
     C                   SETOFF                                       17
     C     ##MGRI        IFEQ      'Y'
     C                   SETON                                        17
     C                   ENDIF
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      Q1USER        XXUSR
     C*
     C     UPKEY         CHAIN(n)  PUP01                              40        SMUPP
     C*
     C     *IN40         IFEQ      '0'
     C*
     C                   MOVE      UPCO#         SCCO#
     C                   MOVE      UPCO#         KKCO#
     C*
     C     COKY          CHAIN     PCO01                              99
     C*
     C                   MOVEL     CONAME        SCCNAM
     C*
     C                   MOVE      UPSUB#        SCSUB#
     C                   MOVE      UPSUB#        KKSUB#
     C*
     C     SMKY          CHAIN     PSM02                              99
     C*
     C                   MOVEL     SMNAME        SCSNAM
     C*
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFND         KKFND
     C*
     C     FNKY2         CHAIN     PFN02                              99
     C*
     C                   MOVEL     FNNAME        SCGNAM
     C*
     C                   MOVE      UPEMP#        SCEMP#
     C                   MOVE      UPDIV         SCDIV
     C                   MOVE      UPEMP#        KKEMP#
     C                   MOVE      UPDIV         KKDIV
     C*
     C     EMKY          CHAIN     PEM02                              99
     C*
     C                   MOVEL     EMNAME        SCENAM
     C*
     C                   MOVE      UPOUTQ        SCOUTQ
     C***  begin add  ***  03/01/16  **************************************
     C                   Move      UPPKey        scPrtKey
     C***  end add ******  03/01/16  **************************************
     C                   Z-ADD     UPFYR         SCYR
     C*
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT
     C*
     C* PROTECT FIELDS THAT ARE RESTRICTED TO THE USER.
     C*
     C                   MOVEA     '0000'        *IN(21)
     C*
     C* SUBSCRIBER RESTRICTIONS
     C*
     C* Check for existence of SMAZP records.
     C*   allow entry of default Fund/Employer
     C*
     C                   CALL      'SA008R'
     C                   PARM                    SCCO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'M'           ##APPL            1
     C                   PARM      *OFF          ##RCDS            1
     C                   PARM      *BLANK        ##AUTH            1
     C     UPMRSU        IFEQ      'Y'
     C     ##RCDS        ANDEQ     *OFF
     C     *IN17         ANDEQ     *ON
     C     UPMRSU        OREQ      'Y'
     C     *IN17         ANDEQ     *OFF
     C*
     C                   SETON                                        21
     C*
     C                   ENDIF
     C*
     C* GROUP RESTRICTIONS
     C*
     C     UPMRGR        IFEQ      'Y'
     C     ##RCDS        ANDEQ     *OFF
     C     *IN17         ANDEQ     *ON
     C     UPMRGR        OREQ      'Y'
     C     *IN17         ANDEQ     *OFF
     C*
     C                   SETON                                        22
     C*
     C                   ENDIF
     C*
     C* EMPLOYER/DIVISION RESTRICTIONS
     C*
     C     UPMREM        IFEQ      'Y'
     C     ##RCDS        ANDEQ     *OFF
     C     *IN17         ANDEQ     *ON
     C     UPMREM        OREQ      'Y'
     C     *IN17         ANDEQ     *OFF
     C*
     C                   SETON                                        23
     C     UPFL5         COMP      'Y'                                    24
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVE      *ZERO         SCCO#
     C                   MOVE      *BLANKS       SCCNAM
     C                   MOVE      *ZERO         SCSUB#
     C                   MOVE      *BLANKS       SCSNAM
     C                   MOVE      *ZERO         SCFND
     C                   MOVE      *BLANKS       SCGNAM
     C                   MOVE      *ZERO         SCEMP#
     C                   MOVE      *ZERO         SCDIV
     C                   MOVE      *BLANKS       SCENAM
     C                   MOVE      *BLANK        SCOUTQ
     C                   MOVE      *ZERO         SCYR
     C                   MOVE      *ZERO         SCDT
     C*
     C                   END
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3 = EXIT
     C     *INKC         IFEQ      '1'
     C     *INKL         OREQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*____________________________________________________________________
     C*
     C* F10=SEARCH                     *
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C                   CLEAR                   XXPGM
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCCO#'
     C                   MOVEL(P)  'S0510R'      XXPGM
     C     HFLD          WHENEQ    'SCSUB#'
     C                   MOVEL(P)  'F0055R'      XXPGM
     C******** Beg Del ***** 12/18/24 ***************************
     C**** HFLD          WHENEQ    'SCFND'
     C****               MOVEL(P)  'F0060R'      XXPGM
     C******** End Del ***** 12/18/24 ***************************
     C     HFLD          WHENEQ    'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C                   MOVEL(P)  'F0140R'      XXPGM
     C                   ENDSL
     C*
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM                    XXPGM            10
     C                   PARM                    XXERR             1
     C*
     C                   SETOFF                                       16
     C*
     C     XXERR         IFEQ      *ON
     C     XXPGM         ANDNE     *BLANKS
     C*
     C* MAKE DUPLICATE PROGRAM
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCCO#'
     C*
     C                   CALL      'FA711C'
     C                   PARM      *BLANKS       ##RTN             1
     C                   PARM      'S0510R'      ##FNM            10
     C                   PARM      'S0510Q'      ##TNM            10
     C*
     C     HFLD          WHENEQ    'SCSUB#'
     C*
     C                   CALL      'FA711C'
     C                   PARM      *BLANKS       ##RTN             1
     C                   PARM      'F0055R'      ##FNM            10
     C                   PARM      'F0055Q'      ##TNM            10
     C*
     C******** Beg Del ***** 12/18/24 ***************************
     C**** HFLD          WHENEQ    'SCFND'
     C*
     C****               CALL      'FA711C'
     C****               PARM      *BLANKS       ##RTN             1
     C****               PARM      'F0060R'      ##FNM            10
     C****               PARM      'F0060Q'      ##TNM            10
     C******** End Del ***** 12/18/24 ***************************
     C*
     C     HFLD          WHENEQ    'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C                   CALL      'FA711C'
     C                   PARM      *BLANKS       ##RTN             1
     C                   PARM      'F0140R'      ##FNM            10
     C                   PARM      'F0140Q'      ##TNM            10
     C*
     C                   ENDSL
     C*
     C     ##RTN         IFEQ      'A'
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   SETON                                        16
     C*
     C                   ENDIF                                                  XXERR=*ON
     C*
     C**********************************
     C* CO# SEARCH                     *
     C**********************************
     C*
     C     *IN21         IFEQ      *OFF
     C     *IN22         ANDEQ     *OFF
     C     *IN23         ANDEQ     *OFF
     C     HFLD          IFEQ      'SCCO#'
     C*
     C     *IN16         IFEQ      *OFF
     C*
     C                   CALL      'S0501R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#
     C                   PARM                    QQRTCD            1
     C                   ELSE
     C*
     C                   CALL      'S0501Q'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#
     C                   PARM                    QQRTCD            1
     C*
     C                   ENDIF
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQCO#         OREQ      *ZERO
     C*
     C                   SETON                                        30
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      '*'
     C*
     C                   MOVE      QQCO#         SCCO#
     C                   MOVE      QQCO#         KKCO#
     C*
     C     COKY          CHAIN     PCO01                              99
     C*
     C*
     C                   MOVEL     CONAME        SCCNAM
     C*
     C                   SETON                                        31
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  HFLD
     C*
     C                   ELSE
     C*
     C     HFLD          IFEQ      'SCCO#'
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   ENDIF
     C*
     C**********************************
     C*   SUBSCRIBER SEARCH            *
     C**********************************
     C*
     C     *IN21         IFEQ      *OFF
     C     *IN22         ANDEQ     *OFF
     C     *IN23         ANDEQ     *OFF
     C*
     C     HFLD          IFEQ      'SCSUB#'
     C*
     C     *IN16         IFEQ      *OFF
     C*
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   ELSE
     C*
     C                   CALL      'F0055Q'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZERO
     C*
     C                   SETON                                        31
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PSM01                              99
     C*
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVEL     SMNAME        SCSNAM
     C                   SETON                                        32
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKL
     C*
     C                   ENDIF                                                  HFLD
     C*
     C                   ELSE
     C*
     C     HFLD          IFEQ      'SCSUB#'
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*********************************
     C*   GROUP ALPHA SEARCH          *
     C*********************************
     C*
     C******** Beg Del ***** 12/18/24 ***************************
     C******IN22         IFEQ      *OFF
     C******IN23         ANDEQ     *OFF
     C*****
     C*****HFLD          IFEQ      'SCFND'
     C*****
     C*****              MOVE      SCCO#         KKCO#
     C*****              MOVE      SCSUB#        KKSUB#
     C*****
     C*****SMKY          CHAIN     PSM02                              99
     C*****
     C******IN99         IFEQ      *OFF
     C******IN16         IFEQ      *OFF
     C*****
     C*****              CALL      'F0060R'
     C*****              PARM      '2'           ##CALL
     C******INKC         PARM      '0'           ##KC
     C******INKL         PARM      '0'           ##KL
     C*****              PARM      DBRRN         ##RRN
     C****               ELSE
     C*****
     C*****              CALL      'F0060Q'
     C*****              PARM      '2'           ##CALL
     C******INKC         PARM      '0'           ##KC
     C******INKL         PARM      '0'           ##KL
     C*****              PARM      DBRRN         ##RRN
     C*****
     C*****              ENDIF
     C*****
     C******INKL         IFEQ      *ON
     C*****##RRN         OREQ      *ZERO
     C*****
     C*****              SETON                                        32
     C*****              GOTO      STRT
     C*****
     C*****              ELSE
     C*****
     C*****##RRN         CHAIN     PFN01                              99
     C*****
     C*****              MOVE      FNFND         SCFND
     C*****              MOVEL     FNNAME        SCGNAM
     C*****              SETON                                        33
     C*****              GOTO      STRT
     C*****
     C*****              ENDIF
     C*****
     C*****              ELSE
     C*****
     C**N21              SETON                                        71
     C***21              SETON                                        76
     C****               GOTO      STRT
     C*****
     C*****              ENDIF
     C*****
     C*****              ENDIF
     C*****
     C*****              ELSE
     C*****
     C**** HFLD          IFEQ      'SCFND'
     C*****
     C*****              GOTO      STRT
     C*****
     C*****              ENDIF
     C*****
     C*****              ENDIF
     C******** End Del ***** 12/18/24 ***************************
     C*
     C*********************************
     C*   EMPLOYER/DIVISION SEARCH    *
     C*********************************
     C*
     C     *IN23         IFEQ      *OFF
     C*
     C     HFLD          IFEQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C                   MOVE      SCCO#         KKCO#
     C                   MOVE      SCSUB#        KKSUB#
     C                   MOVE      SCFND         KKFND
     C*
     C     FNKY2         CHAIN     PFN02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     *IN16         IFEQ      *OFF
     C*
     C                   CALL      'F0140R'
     C                   PARM      '2'           ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      DBRRN2        ##RRN2            9 0
     C                   PARM      3             ##VIEW            1 0
     C                   ELSE
     C*
     C                   CALL      'F0140Q'
     C                   PARM      '2'           ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      DBRRN2        ##RRN2            9 0
     C                   PARM      3             ##VIEW            1 0
     C*
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZERO
     C*
     C                   SETON                                        33
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PEM01                              99
     C*
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   MOVEL     EMNAME        SCENAM
     C*
     C                   SETON                                        34
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C  N22              SETON                                        72
     C   22              SETON                                        77
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*====================================================================
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C     SCR01T        TAG
     C*
     C*  THE COMPANY NUMBER MUST BE VALID
     C*
     C                   Z-ADD     SCCO#         KKCO#
     C*
     C     COKY          CHAIN     PCO01                              99
     C*
     C     *IN99         IFEQ      '1'
     C     COSTAT        OREQ      'I'
     C*
     C                   SETON                                        70
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C* SKIP EDIT ON SUBSCRIBER IF THE FIELD IS PROTECTED.
     C*
     C     *IN21         IFEQ      *OFF
     C*
     C*  THE SUBSCRIBER NUMBER MUST BE VALID
     C*
     C                   Z-ADD     SCCO#         KKCO#
     C                   Z-ADD     SCSUB#        KKSUB#
     C*
     C     SMKY          CHAIN     PSM02                              60
     C*
     C     *IN60         IFEQ      '1'
     C     SMSTAT        OREQ      'I'
     C*
     C                   SETON                                        71
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C                   ENDIF                                                  *IN21 *OFF
     C*
     C* Check for Subscriber Restriction
     C*
     C     ##RCDS        IFEQ      *ON
     C     UPMRSU        ANDEQ     'Y'
     C                   CALL      'SA008R'
     C                   PARM                    SCCO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'M'           ##APPL            1
     C     *IN78         PARM      *OFF          ##RTCD            1
     C                   PARM      *BLANK        ##AUTH            1
     C                   PARM      *ZERO         ##FND
     C                   PARM      *ZERO         ##EMP#
     C                   PARM      SCSUB#        ##SUB#
     C*
     C   78              GOTO      STRT
     C                   ENDIF                                                  UPMRSU
     C*
     C* SKIP EDIT OF GROUP FIELD IF IT IS PROTECTED.
     C*
     C     *IN22         IFEQ      *OFF
     C*
     C*  THE FUND NUMBER MUST BE VALID
     C*
     C                   Z-ADD     SCCO#         KKCO#
     C                   Z-ADD     SCSUB#        KKSUB#
     C                   Z-ADD     SCFND         KKFND
     C*
     C     FNKY          CHAIN     PFN02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     SCSUB#        OREQ      *ZEROS
     C*
     C     FNKY2         CHAIN     PFN02                              99
     C*
     C     *IN99         IFEQ      '1'
     C     FNSTAT        OREQ      'I'
     C*
     C                   SETON                                        72
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF                                                  *IN22 *OFF
     C*
     C* Check for Fund Restriction
     C*
     C     ##RCDS        IFEQ      *ON
     C     UPMRGR        ANDEQ     'Y'
     C                   CALL      'SA008R'
     C                   PARM                    SCCO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'M'           ##APPL            1
     C     *IN79         PARM      *OFF          ##RTCD            1
     C                   PARM      *BLANK        ##AUTH            1
     C                   PARM      SCFND         ##FND
     C                   PARM      *ZERO         ##EMP#
     C                   PARM      SCSUB#        ##SUB#
     C*
     C   79              GOTO      STRT
     C                   ENDIF                                                  UPMRGR
     C*
     C* SKIP EDIT OF EMPLOYER/DIVISION FIELD IF IT IS PROTECTED.
     C*
     C     *IN23         IFEQ      *OFF
     C*
     C*  THE EMPLOYER/DIVISION NUMBER MUST BE VALID IF NOT ZERO.
     C*
     C     SCEMP#        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCCO#         KKCO#
     C                   Z-ADD     SCSUB#        KKSUB#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCEMP#        KKEMP#
     C                   Z-ADD     SCDIV         KKDIV
     C*
     C     EMKY          CHAIN     PEM02                              99
     C*
     C     *IN99         IFEQ      '1'
     C     EMSTAT        OREQ      'I'
     C*
     C                   SETON                                        73
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF                                                  *IN23 *OFF
     C*
     C* Check for Employer Restriction
     C*
     C     ##RCDS        IFEQ      *ON
     C     UPMREM        ANDEQ     'Y'
     C     SCEMP#        ANDNE     *ZEROS
     C*
     C                   CALL      'SA008R'
     C                   PARM                    SCCO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'M'           ##APPL            1
     C     *IN80         PARM      *OFF          ##RTCD            1
     C                   PARM      *BLANK        ##AUTH            1
     C                   PARM      SCFND         ##FND
     C                   PARM      SCEMP#        ##EMP#
     C                   PARM      SCSUB#        ##SUB#
     C*
     C   80              GOTO      STRT
     C                   ENDIF                                                  UPMREM
     C*
     C*   CHECK THE PROCESS DATE FOR A VALID DATE
     C*
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   SETON                                        74
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*  CHECK THE OUTQ FOR A VALID VALUE
     C*
     C                   CALL      'S0003C'
     C                   PARM                    SCOUTQ
     C                   PARM      ' '           XXFOND            1
     C*
     C     XXFOND        IFEQ      '1'
     C*
     C                   SETON                                        75
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     UPKEY         CHAIN     PUP01                              40        SMUPP
     C                   MOVE      Q1USER        UPUSR
     C                   MOVE      SCCO#         UPCO#
     C                   MOVE      SCSUB#        UPSUB#
     C                   MOVE      SCFND         UPFND
     C                   Z-ADD     SCYR          UPFYR
     C                   MOVE      SCOUTQ        UPOUTQ
     C***  begin add  ***  03/01/16  **************************************
     C                   Move      scPrtKey      UPPKey
     C***  end add ******  03/01/16  **************************************
     C                   MOVE      SCEMP#        UPEMP#
     C                   MOVE      SCDIV         UPDIV
     C*
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPPRDT
     C*
     C                   MOVE      Q1USER        UPUSER
     C                   MOVE      Q1PGM         UPPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPDT
     C*
     C                   Z-ADD     DSTIME        UPTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PUP01
     C                   ELSE
     C*
     C                   MOVE      'A'           UPSTAT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPIDT
     C*
     C                   MOVE      Q1USER        UPIUSR                         |
     C                   WRITE     PUP01
     C                   END
     C*
     C     ##WCIF        IFEQ      'Y'
     C     ##NPWC        ANDNE     'Y'
     C                   CALL      'WA025R'                             99
     C                   PARM                    UPUSR
     C                   PARM                    ##DLT             1
     C                   ENDIF
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**  TABLE UN
        CHANGE USER PARAMETERS
