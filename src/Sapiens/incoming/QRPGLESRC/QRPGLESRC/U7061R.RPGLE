     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE U7061R - PROCEDURE/DIAGNOSIS ANALYSIS REPORT
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION:                                         *
     H*    PROMPT PROGRAM FOR PROCEDURE/DIAGNOSIS ANALYSIS REPORT     *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    UTILIZATION REVIEW MENU                                    *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    F0545R         SUBSCRIBER SEARCH                           *
     H*    F0508R         GROUP SEARCH                                *
     H*    F0690R         EMPLOYER SEARCH                             *
     H*    F0032R         PROVIDER SEARCH                             *
     H*                                                               *
     H*  PROGRAMMER: W. DENEHY          PROJECT: 99AAA                *
     H*  CREATED: 01/08/93                                            *
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  12/18/24 C:342 Project : MC447 Phase 2                       *
     H*                 1.F10 Search disabled on the Group Field      *
     H*                 DSPF Changes:                                 *
     H*                 1.Changed Existing Group field into Typeahead *
     H*                                                               *
     H*  11/15/02 C:79  Project: MCMNT                                *
     H*                 Initialize parms on FEIN/MOD search so subset *
     H*                 does not occur on subsequent searches         *
     H*                                                               *
     H*  07/14/98 C:61  Project: 98MAY                                *
     H*                 Restrict access to provider types 1 and 4     *
     H*                 only.                                         *
     H*                                                               *
     H*  05/04/98 C:91  Project: MC181                                *
     H*                 COMBINED F6 & F7 KEYS INTO ONE F10 SEARCH KEY *
     H*                 SWAP F0545R TO F0055R ON SUBSCRIBER SEARCH    *
     H*                 SWAP F0508R TO F0060R ON GROUP SEARCH         *
     H*                 SWAP F0690R TO F0140R ON EMPLOYER SEARCH      *
     H*                 ADDED PROVIDER SEARCH (F0032R)                *
     H*                 ADDED TYPE OF ADMISSION SEARCH (F2380R)       *
     H*                 DELETED CHG USER PARMS                        *
     H*                 CHANGED SCREEN DESIGN TO CURRENT STANDARDS    *
     H*                                                               *
     H*  03/25/98 C:53  Project: MC145                                *
     H*                 SWAP F0533R TO F0032R (NEW VERSION)           *
     H*                                                               *
     H*  12/20/96   C:59     REQ/PRJ: MCMNT                           *
     H*             ADDED NEW PARM TO CALL FOR EMPLOYER SEARCH.  NEW  *
     H*             FIELD WAS CREATED IN MCMOD3 ENVIRONMENT.          *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H* SCR01:                                                        *
     H*  20        POSIT CURSOR @ SUB#                                *
     H*  21        POSIT CURSOR @ GROUP                               *
     H*  22        POSIT CURSOR @ EMP#                                *
     H*  23        POSIT CURSOR @ PROVIDER                            *
     H*  24        POSIT CURSOR @ DATE RANGE                          *
     H*  25        MCR0344-SUBSCRIBER AND PROVIDER CANNOT BE ENTERED  *
     H*            AT THE SAME TIME                                   *
     H*  26        EMP# WAS LEFT BLANK DO NOT VALIDATE                *
     H*  27        MCR0502-VALUE CANNOT BE < OR = BLANKS              *
     H*  28        MCR0301-SUBSCRIBER # IS INVALID                    *
     H*  29        MCR0502-VALUE CANNOT BE < OR = BLANKS              *
     H*  30        MCR0508-GROUP NUMBER IS INVALID                    *
     H*  31        MCR0538-EMPLOYER NUMBER IS INVALID                 *
     H*  32        MCR0333-INVALID PAYEE ENTERED                      *
     H*  33        MCR0500-INVALID DATE IN FIELD                      *
     H*  34        MCR0500-INVALID DATE IN FIELD                      *
     H*  35        MCR0529-DATES MUST BE IN ASCENDING ORDER           *
     H*  36        MCR0345-VALUE MUST BE 'P' OR 'D'                   *
     H*  37        MCR0797-INVALID TYPE OF ADMISSION ENTERED          *
     H*  38        MCR0797-INVALID TYPE OF ADMISSION ENTERED          *
     H*  39        MCR0797-INVALID TYPE OF ADMISSION ENTERED          *
     H*  40        MCR0797-INVALID TYPE OF ADMISSION ENTERED          *
     H*  41        MCR0797-INVALID TYPE OF ADMISSION ENTERED          *
     H*  42        MCR0304-REPORT TYPE MUST BE 'D','S',OR 'B'         *
     H*  43        POSIT CURSOR @ PROV MOD                            *
     H*  44        POSIT CURSOR @ TYPE OF ADMISSION 1                 *
     H*  45        POSIT CURSOR @ TYPE OF ADMISSION 2                 *
     H*  46        POSIT CURSOR @ TYPE OF ADMISSION 3                 *
     H*  47        POSIT CURSOR @ TYPE OF ADMISSION 4                 *
     H*  48        POSIT CURSOR @ TYPE OF ADMISSION 5                 *
     H*  49        POSIT CURSOR @ REPORT TYPE                         *
     H*  50        MC40004-CURSOR NOT A PROPER FIELD FOR SEARCH       *
     H*  60        GENERAL CHAIN                                      *
     H*****************************************************************
     FU7061F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
      **** BEGIN ADD **** 05/04/98 ************************************
     F                                     INFDS(DBFDB)
      **** END   ADD **** 05/04/98 ************************************
     FFMEML     IF   E           K DISK
     FFMSML     IF   E           K DISK
      ***** BEG DEL ******* 07/14/98 **********************************
     F*FMPDL   IF  E           K        DISK
      ***** END DEL ******* 07/14/98 **********************************
      ***** BEG ADD ******* 07/14/98 **********************************
     FFMPDL3    IF   E           K DISK
      ***** END ADD ******* 07/14/98 **********************************
      **** BEGIN ADD **** 05/04/98 ************************************
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
     FFMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     FFTGTP     IF   E             DISK
      **** END   ADD **** 05/04/98 ************************************

     D*
      **** BEGIN DEL **** 05/04/98 ************************************
     D**********          CK1A    1   3 78               CMD KEYS FOR SCR01
      **** END   DEL **** 05/04/98 ************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
      **** BEGIN ADD **** 05/04/98 ************************************
     D DBFDB           DS
     D  DBRRB                397    400B 0
      **** END   ADD **** 05/04/98 ************************************

     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C*
     C*   KEY LIST FOR THE EMPLOYER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C*
     C*   KEY LIST FOR THE PAYEE DEMOGRAPHICS MASTER FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C*
     C     *LIKE         DEFINE    EMDT          XXFRDT
     C     *LIKE         DEFINE    EMDT          XXTODT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   ELSE
     C                   MOVE      UPSUB#        SCSUB#
     C                   MOVE      UPFND         SCFND
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
      **** BEGIN DEL **** 05/04/98 ************************************
     C**********           MOVE Q1USER    SCUSER
      **** END   DEL **** 05/04/98 ************************************
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN22
     C*
      **** BEGIN DEL **** 05/04/98 ************************************
     C**********           Z-ADD1         CK
     C**********           MOVELCK1A,CK   SCCMD1
      **** END   DEL **** 05/04/98 ************************************
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
     C     *IN11         IFEQ      '1'
     C***********************************
     C*   F3 = EXIT                     *
     C***********************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
      **** BEGIN CHG **** 05/04/98 ************************************
     C************************************
     C*   F6                             *
     C************************************
     C********** *INKF     IFEQ '1'
     C************************************
     C*   SCR01 = SUSCRIBER SEARCH       *
     C************************************
     C**********           CALL 'F0545R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCSUB#
     C**********           PARM           QQRTCD  1
     C********** QQRTCD    CABEQ'A'       TERM
     C**********
     C********** QQRTCD    IFEQ 'B'
     C********** SCSUB#    OREQ *ZERO
     C**********           MOVE '1'       *IN20
     C**********           GOTO STRT
     C**********           END                             END EQUAL B
     C**********
     C********** QQRTCD    IFEQ '*'
     C**********           MOVE '1'       *IN21
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           END                             END *
     C**********           END                             END *INKF
     C***********************************
     C*   F7                            *
     C***********************************
     C********** *INKG     IFEQ '1'
     C************************************
     C*   SCR01 = GROUP SEARCH           *
     C************************************
     C**********
     C********** SCSUB#    IFEQ *ZEROS
     C**********           MOVE UPSUB#    QQSUB   50
     C**********           ELSE
     C**********           MOVE SCSUB#    QQSUB
     C**********           END
     C**********           CALL 'F0508R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQSUB
     C**********           PARM           SCFND
     C**********           PARM           QQRTC   1
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTC     IFEQ 'B'
     C********** SCFND     OREQ *ZERO
     C**********           MOVE '1'       *IN21
     C**********           GOTO STRT
     C**********           END                             END EQUAL B
     C**********
     C********** QQRTC     IFEQ '*'
     C**********           MOVE QQSUB     SCSUB#
     C**********           MOVE '1'       *IN22
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           END                             END *
     C**********
     C**********           END                             END *INKG
     C**********************************
     C*   F8                           *
     C**********************************
     C********** *INKH     IFEQ '1'
     C**********************************
     C*   SCR01 = EMPLOYER # SEARCH    *
     C**********************************
     C*** BEGIN ADD *** 12/20/96 *************************************
     C*   ADDED QQDIV TO CALL, UPDATE TO MCMOD3 VERSION ADDED DIV TO
     C*   EMPLOYER SEARCH.
     C**********
     C**********           MOVEL'000'     QQDIV   30
     C*** END ADD ***** 12/20/96 *************************************
     C**********
     C**********           CALL 'F0690R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCEMP#
     C**********           PARM           QQDIV            ADD 12/20/96
     C**********           PARM           QQRTCD  1
     C**********
     C********** QQRTCD    CABEQ'A'       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTCD    IFEQ 'B'
     C********** SCEMP#    OREQ *ZERO
     C**********           MOVE '1'       *IN22
     C**********           GOTO STRT
     C**********           END
     C**********
     C********** QQRTCD    IFEQ '*'
     C**********           MOVE '1'       *IN24
     C**********           GOTO STRT
     C**********           END
     C**********
     C**********           END                             END *INKH
     C**********************************
     C*   F9                           *
     C**********************************
     C********** *INKI     IFEQ '1'
     C**********************************
     C*   SCR01 = PROVIDER SEARCH      *
     C**********************************
     C******************   CALL 'F0533R'                   DEL 03/25/98
     C**********           MOVE SCFEIN    XXFEIN  9
     C**********           MOVE SCMOD     XXMOD   3
     C**********           CALL 'F0032R'                   ADD 03/25/98
     C**********           PARM           QQPRDS           DEL 03/25/98
     C**********           PARM '2'       QQCALL           CHG 03/25/98
     C**********           PARM           XXFEIN
     C**********           PARM           XXMOD
     C**********           PARM           QQRTCD  1
     C**********           PARM ' '       QQTYPE  1        ADD 03/25/98
     C**********
     C********** QQRTCD    CABEQ'A'       TERM
     C**********
     C**********           MOVE XXFEIN    SCFEIN
     C**********           MOVE XXMOD     SCMOD
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTCD    IFEQ 'B'
     C********** SCFEIN    OREQ *ZERO
     C**********           MOVE '1'       *IN23
     C**********           GOTO STRT
     C**********           END
     C**********
     C********** QQRTCD    IFEQ '*'
     C**********           MOVE '1'       *IN24
     C**********           GOTO STRT
     C**********           END
     C**********
     C**********           END                             END *INKI
     C****************************************
     C*      F24 = MORE COMMAND KEYS         *
     C****************************************
     C********** *INKY     IFEQ '1'
     C**********           MOVELQ2RECF    HLRECF 10
     C********** CK        ADD  1         CK      50
     C**********
     C********** HLRECF    IFEQ 'SCR01'
     C********** CK        IFGT 2
     C**********           Z-ADD1         CK
     C**********           END                             END CK GT 2
     C**********           MOVELCK1A,CK   SCCMD1
     C**********           END                             END SCR01
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVELHLRECF    Q2RECF
     C**********           GOTO STRT
     C**********           END                             END *INKY
     C**************************************
     C*   PROCESS CHANGE OF USER PARAMETERS*
     C**************************************
     C********** *INKM     IFEQ '1'
     C**********
     C**********           CALL 'F0006R'
     C**********
     C*  GET THE USER PARAMETER FILE
     C**********
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C**********           MOVE *ZERO     UPCO#
     C**********           MOVE *ZERO     UPSUB#
     C**********           ELSE
     C**********           MOVE UPSUB#    SCSUB#
     C**********           MOVE UPFND     SCFND
     C**********           END
     C**********           GOTO STRT01
     C**********           END
     C**********
     C*
      ***************************
      * F10 = SEARCH            *
      ***************************
     C     *INKJ         IFEQ      *ON
     C     Q2RECF        IFEQ      'SCR01'
     C*
      *********************
      * SUBSCRIBER SEARCH *
      *********************
     C     HFLD          IFEQ      'SCSUB#'
     C*
     C                   CALL      'F0055R'
     C*
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN             9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     20     PC AT SUB#
     C*
     C     ##RRN         CHAIN     PSM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVE      *ON           *IN21                          PC AT SCFND
     C                   ELSE
     C                   MOVE      *ON           *IN20                          PC AT SCSUB#
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCSUB#
     C*
      ****************
      * GROUP SEARCH *
      ****************
     C     HFLD          IFEQ      'SCFND'
     C******** Beg Del ***** 12/18/24 ***************************
     C*****
     C*****              MOVE      ##RRN         ##RRN2
     C*****
     C*****              CALL      'F0060R'
     C*****
     C*****              PARM      '2'           ##CALL            1
     C******INKC         PARM                    ##KC              1
     C******INKL         PARM                    ##KL              1
     C*****              PARM                    ##RRN2            9 0
     C*****
     C******INKC         CABEQ     *ON           TERM
     C******INKL         CABEQ     *ON           STRT                     21     PC AT SCFND
     C*****
     C*****##RRN2        CHAIN     PFN02                              60
     C*****
     C******IN60         IFEQ      *OFF
     C*****              MOVE      FNFND         SCFND
     C*****              MOVE      *ON           *IN22                          PC AT SCEMP#
     C*****              ELSE
     C*****              MOVE      *ON           *IN21                          PC AT SCFND
     C*****              ENDIF                                                  *IN60 IFEQ
     C******** End Del ***** 12/18/24 ***************************
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCFND
     C*
      ********************
      * EMPLOYER SEARCH  *
      ********************
     C     HFLD          IFEQ      'SCEMP#'
     C*
     C     SCFND         IFNE      0
     C     FNKEY         CHAIN     PFN01                              60
     C                   Z-ADD     DBRRB         ##RRN3
     C                   ENDIF                                                  SCFND EQ 0
     C*
     C                   CALL      'F0140R'
     C*
     C                   PARM      '2'           ##CALL
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##RRN                          FMEMP
     C                   PARM                    ##RRN3            9 0          FMFNP
     C                   PARM                    ##VIEW            1 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     22     PC AT EMP#
     C*
     C     ##RRN         CHAIN     PEM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      *ON           *IN24                          PC AT SCFRDT
     C                   ELSE
     C                   MOVE      *ON           *IN22                          PC AT SCEMP#
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCFND
     C*
      ********************
      * PROVIDER SEARCH  *
      ********************
     C     HFLD          IFEQ      'SCFEIN'
     C     HFLD          OREQ      'SCMOD'
     C*
     C                   CALL      'F0032R'
     C                   PARM      '2'           ##CALL            1
     C*** BEG CHG *** 11/15/02 ****************************************
     C******************** PARM           ##FEIN  9
     C******************** PARM           ##MOD   3
     C******************** PARM           ##RTCD  1
     C******************** PARM           ##TYPE  1
     C                   PARM      *BLANKS       ##FEIN            9
     C                   PARM      *BLANKS       ##MOD             3
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM      *BLANK        ##TYPE            1
     C*** END CHG *** 11/15/02 ****************************************
      ***** BEG ADD ******* 07/14/98 **********************************
     C                   PARM      'Y'           ##1N4             1            type 1 & 4 only
      ***** END ADD ******* 07/14/98 **********************************
     C*
     C     ##RTCD        CABEQ     'A'           TERM
     C*
     C     ##RTCD        IFEQ      'B'
     C*** BEG DEL *** 11/15/02 ****************************************
     C********** ##MOD     OREQ *ZERO
     C*** END DEL *** 11/15/02 ****************************************
     C     HFLD          CABEQ     'SCFEIN'      STRT                     23      PC AT PROVR
     C     HFLD          CABEQ     'SCMOD'       STRT                     43      PC AT MOD SE
     C                   ENDIF                                                  ##RTCD
     C*
     C     ##RTCD        IFEQ      '*'
     C                   MOVE      ##FEIN        SCFEIN
     C                   MOVE      ##MOD         SCMOD
     C                   MOVE      *ON           *IN24                          PC AT SCFRDT
     C                   GOTO      STRT
     C                   ENDIF                                                  QQRTC EQ *
     C*
     C                   ENDIF                                                  SCFEIN SCMOD
     C*
      *****************************
      * TYPE OF ADMISSION SEARCH  *
      *****************************
     C     HFLD          IFEQ      'SCATP1'
     C     HFLD          OREQ      'SCATP2'
     C     HFLD          OREQ      'SCATP3'
     C     HFLD          OREQ      'SCATP4'
     C     HFLD          OREQ      'SCATP5'
     C*
     C                   CALL      'F2380R'
     C*
     C                   PARM      '2'           ##CALL
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##RRN             9 0
     C                   PARM      'TYAD'        ##TBID            4
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      0
     C     HFLD          CABEQ     'SCATP1'      STRT                     44     PC @ SCATP1
     C     HFLD          CABEQ     'SCATP2'      STRT                     45     PC @ SCATP2
     C     HFLD          CABEQ     'SCATP3'      STRT                     46     PC @ SCATP3
     C     HFLD          CABEQ     'SCATP4'      STRT                     47     PC @ SCATP4
     C     HFLD          CABEQ     'SCATP5'      STRT                     48     PC @ SCATP5
     C                   ENDIF                                                   *INKL
     C*
     C     ##RRN         CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   SELECT
     C     HFLD          WHENEQ    'SCATP1'
     C                   MOVEL     GTKEY         SCATP1
     C                   MOVE      *ON           *IN45                          PC @ SCATP2
     C     HFLD          WHENEQ    'SCATP2'
     C                   MOVEL     GTKEY         SCATP2
     C                   MOVE      *ON           *IN46                          PC @ SCATP3
     C     HFLD          WHENEQ    'SCATP3'
     C                   MOVEL     GTKEY         SCATP3
     C                   MOVE      *ON           *IN47                          PC @ SCATP4
     C     HFLD          WHENEQ    'SCATP4'
     C                   MOVEL     GTKEY         SCATP4
     C                   MOVE      *ON           *IN48                          PC @ SCATP5
     C     HFLD          WHENEQ    'SCATP5'
     C                   MOVEL     GTKEY         SCATP5
     C                   MOVE      *ON           *IN49                          PC @ SCTYPE
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  IFEQ *IN60
     C*
     C                   ENDIF                                                  SCATP
     C*
     C                   MOVE      *ON           *IN50                          ERROR
     C*
     C                   ENDIF                                                  SCR01
     C                   ENDIF                                                  INKJ
     C*
      **** END   CHG **** 05/04/98 ************************************
     C                   ENDIF                                                  IN11
     C***************************
     C*   PROCESS REQUEST SCREEN*
     C***************************
     C     SCR01T        TAG
     C*****************************************************************
     C*
     C*  VALIDATE SUBSCRIBER OR PROVIDER
     C*
     C                   MOVE      '0'           *IN26
     C*
     C*  SUBSCRIBER AND PROVIDER CANNOT BE ENTERED AT THE SAME TIME
     C*
     C     SCSUB#        IFNE      *ZEROS
     C     SCFEIN        ANDNE     *ZEROS
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C     SCFND         IFNE      *ZEROS
     C     SCFEIN        ANDNE     *ZEROS
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C     SCEMP#        IFNE      *ZEROS
     C     SCFEIN        ANDNE     *ZEROS
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*  DETERMINE WHAT WAS ENTERED
     C*
     C     SCEMP#        IFEQ      *ZEROS
     C     SCFEIN        ANDEQ     *ZEROS
     C                   MOVE      '1'           *IN26
     C                   END
     C*
     C     SCFEIN        IFEQ      *ZEROS
     C*
     C*  VALIDATE THE SUBSCRIBER
     C*
     C     SCSUB#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN28                          INVALID SUB#
     C                   GOTO      STRT
     C                   END                                                    END IF *IN60
     C*
     C*  VALIDATE THE GROUP
     C*
     C     SCFND         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN30                          INVALID FUND
     C                   GOTO      STRT
     C                   END                                                    END IF *IN60
     C*
     C*   VALIDATE THE EMPLOYER NUMBER
     C*
     C     *IN27         IFEQ      '0'
     C     SCEMP#        IFNE      *ZEROS
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END                                                    END IF *IN60
     C                   END                                                    END IF SCEMP#
     C                   END                                                    END IF *IN27
     C*
     C                   MOVE      '0'           *IN23
     C*
     C*  VALIDATE THE PROVIDER
     C*
     C                   ELSE
     C*
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN32                          INVALID FEIN
     C                   GOTO      STRT
     C                   END                                                    END IF *IN60
     C*
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN21
     C                   MOVE      '0'           *IN22
     C*
     C                   END
     C*****************************************************************
     C*
     C*  THE FROM DATE CANNOT BE BLANK AND IT MUST BE A VALID DATE
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE TO DATE CANNOT BE BLANK AND IT MUST BE A VALID DATE
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C                   END
     C*
     C*  DATES MUST BE IN ASCENDING ORDER
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C     XXFRDT        IFGT      XXTODT
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   END
     C*
     C* VALIDATE THE SELECTION
     C*
     C     SCSEL         IFNE      'P'
     C     SCSEL         ANDNE     'D'
     C                   MOVE      '1'           *IN36
     C                   GOTO      STRT
     C                   END
     C*
     C* VALIDATE THE TYPE OF ADMISSION
     C*
     C     SCATP1        IFNE      ' '
     C     SCATP1        ANDNE     'M'
     C     SCATP1        ANDNE     'S'
     C     SCATP1        ANDNE     'Y'
     C     SCATP1        ANDNE     'P'
     C     SCATP1        ANDNE     'C'
     C                   MOVE      '1'           *IN37
     C                   GOTO      STRT
     C                   END
     C*
     C     SCATP2        IFNE      ' '
     C     SCATP2        ANDNE     'M'
     C     SCATP2        ANDNE     'S'
     C     SCATP2        ANDNE     'Y'
     C     SCATP2        ANDNE     'P'
     C     SCATP2        ANDNE     'C'
     C                   MOVE      '1'           *IN38
     C                   GOTO      STRT
     C                   END
     C*
     C     SCATP3        IFNE      ' '
     C     SCATP3        ANDNE     'M'
     C     SCATP3        ANDNE     'S'
     C     SCATP3        ANDNE     'Y'
     C     SCATP3        ANDNE     'P'
     C     SCATP3        ANDNE     'C'
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   END
     C*
     C     SCATP4        IFNE      ' '
     C     SCATP4        ANDNE     'M'
     C     SCATP4        ANDNE     'S'
     C     SCATP4        ANDNE     'Y'
     C     SCATP4        ANDNE     'P'
     C     SCATP4        ANDNE     'C'
     C                   MOVE      '1'           *IN40
     C                   GOTO      STRT
     C                   END
     C*
     C     SCATP5        IFNE      ' '
     C     SCATP5        ANDNE     'M'
     C     SCATP5        ANDNE     'S'
     C     SCATP5        ANDNE     'Y'
     C     SCATP5        ANDNE     'P'
     C     SCATP5        ANDNE     'C'
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C                   END
     C*
     C* VALIDATE THE REPORT TYPE
     C*
     C     SCTYPE        IFNE      'D'
     C     SCTYPE        ANDNE     'B'
     C     SCTYPE        ANDNE     'S'
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCSUB#        QQSUB#            5
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCEMP#        QQEMP#            5
     C                   MOVE      SCFEIN        QQFEIN            9
     C                   MOVE      SCMOD         QQMOD             3
     C                   MOVE      SCFRDT        QQFRDT            8
     C                   MOVE      SCTODT        QQTODT            8
     C                   MOVE      SCSEL         QQSEL             1
     C                   MOVE      SCATP1        QQATP1            1
     C                   MOVE      SCATP2        QQATP2            1
     C                   MOVE      SCATP3        QQATP3            1
     C                   MOVE      SCATP4        QQATP4            1
     C                   MOVE      SCATP5        QQATP5            1
     C                   MOVE      SCTYPE        QQTYPE            1
     C*
     C                   CALL      'U7061C'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQEMP#
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C                   PARM                    QQSEL
     C                   PARM                    QQATP1
     C                   PARM                    QQATP2
     C                   PARM                    QQATP3
     C                   PARM                    QQATP4
     C                   PARM                    QQATP5
     C                   PARM                    QQTYPE
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      **** BEGIN DEL **** 05/04/98 ************************************
     C***** CK1A - COMMAND KEYS for SCR01
      ***** F3=Exit F6=Subscriber Search F7=Group Search F24=More       keys
      ***** F8=Emplr Search F9=Prov Search F13=Chg User Parms F24=More  keys
      **** END   DEL **** 05/04/98 ************************************
