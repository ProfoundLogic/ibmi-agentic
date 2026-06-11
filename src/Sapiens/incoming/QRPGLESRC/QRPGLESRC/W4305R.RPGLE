     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W4305R - ASSIGN/CHANGE POLICY STAFF CODE - PROMPT
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*     THIS PROGRAM PROMPTS THE USER TO ENTER POLICY STAFF CODES *
     H*     TO BE ASSIGNED TO A RANGE OF EMPLOYERS OR TO BE CHANGED   *
     H*     WITHIN THOSE EMPLOYERS.                                   *
     H*                                                               *
     H*  COMMENTS                                                     *
     H*                                                               *
     H*    * ADDING OR MOVING FIELDS- THIS PROGRAM HAS 3 SCREENS. ONE *
     H*        FOR ENTERING STAFF CODES, ONE FOR ENTERING SELECTION   *
     H*        CRITERIA, AND THE 3RD FOR UP TO 10 GROUPS.  THERE ARE  *
     H*        COMPILE TIME TABLES FOR INDICATORS FOR POSITIONING THE *
     H*        CURSOR AFTER F10 IS SELECTED AND DURING SCREEN FIELD   *
     H*        VALIDATION.  IF YOU ADD TOO OR HAVE TO RELOCATE A      *
     H*        FIELD YOU MUST MODIFY THE TABLES. THE TABLES MUST      *
     H*        REMAIN IN SYNCH,THAT IS,FIELD ONE IN THE FIELD TABLE   *
     H*        MUST CORRESPOND TO INDICATOR ONE IN THE INDICATOR TABLE*
     H*                                                               *
     H*    EXAMPLE:                                                   *
     H*                                                               *
     H*         FIELD TABLE   INDICATOR TABLE   INDEX                 *
     H*         -----------   ---------------   -----                 *
     H*         SCFREA        20                1                     *
     H*         SCTOEA        21                2                     *
     H*            .          .                 .                     *
     H*            .          .                 .                     *
     H*         SCALLF        29                10                    *
     H*                                                               *
     H*   YOU MUST ALSO CHANGE THE TABLE SIZE IN THE E-SPECS AND      *
     H*   POSSIBLY THE INDEXES IN THE CODE.                           *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     MENU OPTION - QQCALL DETERMINES ASSIGNMENT OR CHANGE.     *
     H*  CALLS TO:                                                    *
     H*     W4305C - SUBMITTER                                        *
     H*                                                               *
     H*  PROGRAMMER: MARIE NGUYEN                                     *
     H*  CREATED:    14/06/93               PROJECT: AVI              *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*      11/22/13 C:190 PROJECT: 129WSB                           *
     H*        - Add field for from and to Claim Contact              *
     H*                                                               *
     H*      12/23/11 C:279 Support: 78800                            *
     H*        -Allow the 'From' field to leave blank.                *
     H*                                                               *
     H*      01/12/98 C:79 PROJECT: WC707                             *
     H*        -DEFAULT THE "RUN FOR ALL FUNDS" FLAG TO "N".          *
     H*                                                               *
     H*      10/30/97 C:79 PROJECT: WC707                             *
     H*        -VR3 IMPLEMENATION, REMOVE THE WORK 'CLERK', ADD AUDIT *
     H*         CONTROL MANAGER AND LOSS CONTROL REP.                 *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H* SCREEN 1:                                                     *
     H*   20   DSPATR(PC) - POSITION CURSOR                ** SCFUWR  *
     H*   21   DSPATR(PC) - POSITION CURSOR                ** SCTUWR  *
     H*   22   DSPATR(PC) - POSITION CURSOR                ** SCFPSV  *
     H*   23   DSPATR(PC) - POSITION CURSOR                ** SCTPSV  *
     H*   24   DSPATR(PC) - POSITION CURSOR                ** SCFPAC  *
     H*   25   DSPATR(PC) - POSITION CURSOR                ** SCTPAC  *
     H*   26   DSPATR(PC) - POSITION CURSOR                ** SCFARV  *
     H*   27   DSPATR(PC) - POSITION CURSOR                ** SCTARV  *
     H*   28   DSPATR(PC) - POSITION CURSOR                ** SCFAMG  *
     H*   29   DSPATR(PC) - POSITION CURSOR                ** SCTAMG  *
     H*   30   DSPATR(PC) - POSITION CURSOR                ** SCFLRP  *
     H*   31   DSPATR(PC) - POSITION CURSOR                ** SCTLRP  *
     H*   32   WCR0513 - INVALID TABLE VALUE...            ** SCFUWR  *
     H*   33   WCR0513 - INVALID TABLE VALUE...            ** SCTUWR  *
     H*   34   WCR0513 - INVALID TABLE VALUE...            ** SCFPSV  *
     H*   35   WCR0513 - INVALID TABLE VALUE...            ** SCTPSV  *
     H*   36   WCR0513 - INVALID TABLE VALUE...            ** SCFPAC  *
     H*   37   WCR0513 - INVALID TABLE VALUE...            ** SCTPAC  *
     H*   38   WCR0513 - INVALID TABLE VALUE...            ** SCFARV  *
     H*   39   WCR0513 - INVALID TABLE VALUE...            ** SCTARV  *
     H*   40   WCR0513 - INVALID TABLE VALUE...            ** SCFAMG  *
     H*   41   WCR0513 - INVALID TABLE VALUE...            ** SCTAMG  *
     H*   42   WCR0513 - INVALID TABLE VALUE...            ** SCFLRP  *
     H*   43   WCR0513 - INVALID TABLE VALUE...            ** SCTLRP  *
     H*   44   WC30613 - A FROM AND TO VALUE MUST BE...    ** SCFUWR  *
     H*   45   WC30613 - A FROM AND TO VALUE MUST BE...    ** SCTUWR  *
     H*   46   WC30613 - A FROM AND TO VALUE MUST BE...    ** SCFPSV  *
     H*   47   WC30613 - A FROM AND TO VALUE MUST BE...    ** SCTPSV  *
     H*   48   WC30613 - A FROM AND TO VALUE MUST BE...    ** SCFPAC  *
     H*   49   WC30613 - A FROM AND TO VALUE MUST BE...    ** SCTPAC  *
     H*   50   WC30613 - A FROM AND TO VALUE MUST BE...    ** SCFARV  *
     H*   51   WC30613 - A FROM AND TO VALUE MUST BE...    ** SCTARV  *
     H*   52   WC30613 - A FROM AND TO VALUE MUST BE...    ** SCFAMG  *
     H*   53   WC30613 - A FROM AND TO VALUE MUST BE...    ** SCTAMG  *
     H*   54   WC30613 - A FROM AND TO VALUE MUST BE...    ** SCFLRP  *
     H*   55   WC30613 - A FROM AND TO VALUE MUST BE...    ** SCTLRP  *
     H*   56   WCR1130 - AT LEAST ONE SELECTION MUST ...   ** SCFUWR  *
     H*  N99   ASSIGN STAFF CODES                                     *
     H*   99   CHANGE STAFF CODES                                     *
     H*                                                               *
     H* SCREEN 2:                                                     *
     H*   20   DSPATR(PC) - POSITION CURSOR                ** SCFREN  *
     H*   21   DSPATR(PC) - POSITION CURSOR                ** SCTOEN  *
     H*   22   DSPATR(PC) - POSITION CURSOR                ** SCFREA  *
     H*   23   DSPATR(PC) - POSITION CURSOR                ** SCTOEA  *
     H*   24   DSPATR(PC) - POSITION CURSOR                ** SCFCHP  *
     H*   25   DSPATR(PC) - POSITION CURSOR                ** SCTCHP  *
     H*   26   DSPATR(PC) - POSITION CURSOR                ** SCFRGN  *
     H*   27   DSPATR(PC) - POSITION CURSOR                ** SCTRGN  *
     H*   28   DSPATR(PC) - POSITION CURSOR                ** SCFEZP  *
     H*   29   DSPATR(PC) - POSITION CURSOR                ** SCTEZP  *
     H*   30   DSPATR(PC) - POSITION CURSOR                ** SCFCTY  *
     H*   31   DSPATR(PC) - POSITION CURSOR                ** SCTCTY  *
     H*   32   DSPATR(PC) - POSITION CURSOR                ** SCFANP  *
     H*   33   DSPATR(PC) - POSITION CURSOR                ** SCTANP  *
     H*   34   DSPATR(PC) - POSITION CURSOR                ** SCFRAA  *
     H*   35   DSPATR(PC) - POSITION CURSOR                ** SCTOAA  *
     H*   36   DSPATR(PC) - POSITION CURSOR                ** SCFRAN  *
     H*   37   DSPATR(PC) - POSITION CURSOR                ** SCTOAN  *
     H*   38   DSPATR(PC) - POSITION CURSOR                ** SCFAZP  *
     H*   39   DSPATR(PC) - POSITION CURSOR                ** SCTAZP  *
     H*   41   WC30610 - INVALID SEARCH FIELD              ** SCFEZP  *
     H*   42   WC30610 - INVALID SEARCH FIELD              ** SCTEZP  *
     H*   43   WC30610 - INVALID SEARCH FIELD              ** SCFANP  *
     H*   44   WC30610 - INVALID SEARCH FIELD              ** SCTANP  *
     H*   45   WC30610 - INVALID SEARCH FIELD              ** SCFAZP  *
     H*   46   WC30610 - INVALID SEARCH FIELD              ** SCTAZP  *
     H*   47   WCR1106 - VALUE MUST BE A,C,B               ** SCACST  *
     H*   48   WC30626 - ENTER NUMBER OR NAME RANGE        ** SCFREN  *
     H*   49   WCR0750 - FROM MUST BE < TO VALUE           ** SCFREA  *
     H*   50   WCR0750 - FROM MUST BE < TO VALUE           ** SCFREN  *
     H*   51   WCR0750 - FROM MUST BE < TO VALUE           ** SCFCHP  *
     H*   52   WCR0750 - FROM MUST BE < TO VALUE           ** SCFRGN  *
     H*   53   WCR0750 - FROM MUST BE < TO VALUE           ** SCFEZP  *
     H*   54   WCR0750 - FROM MUST BE < TO VALUE           ** SCFCHP  *
     H*   55   WCR0750 - FROM MUST BE < TO VALUE           ** SCFCTY  *
     H*   56   WCR0750 - FROM MUST BE < TO VALUE           ** SCFANP  *
     H*   57   WCR0750 - FROM MUST BE < TO VALUE           ** SCFRAA  *
     H*   58   WCR0750 - FROM MUST BE < TO VALUE           ** SCFRAN  *
     H*   59   WCR0750 - FROM MUST BE < TO VALUE           ** SCFAZP  *
     H*   61   DSPATR(PC) - POSITION CURSOR                ** SCALLF  *
     H*   70   WC30610 - INVALID SEARCH FIELD              ** SCALLF  *
     H*   71   WCR0501 - VALUE MUST BE Y OR N              ** SCALLF  *
     H*                                                               *
     H* SCREEN 3:                                                     *
     H*   20   DSPATR(PC) - POSITION CURSOR                ** SCFD01  *
     H*   21   DSPATR(PC) - POSITION CURSOR                ** SCFD02  *
     H*   22   DSPATR(PC) - POSITION CURSOR                ** SCFD03  *
     H*   23   DSPATR(PC) - POSITION CURSOR                ** SCFD04  *
     H*   24   DSPATR(PC) - POSITION CURSOR                ** SCFD05  *
     H*   25   DSPATR(PC) - POSITION CURSOR                ** SCFD06  *
     H*   26   DSPATR(PC) - POSITION CURSOR                ** SCFD07  *
     H*   27   DSPATR(PC) - POSITION CURSOR                ** SCFD08  *
     H*   28   DSPATR(PC) - POSITION CURSOR                ** SCFD09  *
     H*   29   DSPATR(PC) - POSITION CURSOR                ** SCFD10  *
     H*   30   WCR0513 - INVALID TABLE VALUE...            ** SCFD01  *
     H*   31   WCR0513 - INVALID TABLE VALUE...            ** SCFD02  *
     H*   32   WCR0513 - INVALID TABLE VALUE...            ** SCFD03  *
     H*   33   WCR0513 - INVALID TABLE VALUE...            ** SCFD04  *
     H*   34   WCR0513 - INVALID TABLE VALUE...            ** SCFD05  *
     H*   35   WCR0513 - INVALID TABLE VALUE...            ** SCFD06  *
     H*   36   WCR0513 - INVALID TABLE VALUE...            ** SCFD07  *
     H*   37   WCR0513 - INVALID TABLE VALUE...            ** SCFD08  *
     H*   38   WCR0513 - INVALID TABLE VALUE...            ** SCFD09  *
     H*   39   WCR0513 - INVALID TABLE VALUE...            ** SCFD10  *
     H*   40   WC30644 - DUPLICATE GROUP # ENTERED         ** SCFD01  *
     H*   41   WC30644 - DUPLICATE GROUP # ENTERED         ** SCFD02  *
     H*   42   WC30644 - DUPLICATE GROUP # ENTERED         ** SCFD03  *
     H*   43   WC30644 - DUPLICATE GROUP # ENTERED         ** SCFD04  *
     H*   44   WC30644 - DUPLICATE GROUP # ENTERED         ** SCFD05  *
     H*   45   WC30644 - DUPLICATE GROUP # ENTERED         ** SCFD06  *
     H*   46   WC30644 - DUPLICATE GROUP # ENTERED         ** SCFD07  *
     H*   47   WC30644 - DUPLICATE GROUP # ENTERED         ** SCFD08  *
     H*   48   WC30644 - DUPLICATE GROUP # ENTERED         ** SCFD09  *
     H*   49   WC30644 - DUPLICATE GROUP # ENTERED         ** SCFD10  *
     H*                                                               *
     H* MISCELLANEOUS                                                 *
     H*   60   CHAIN - GENERAL CHAIN INDICATOR                        *
     H*****************************************************************
     H*                                                               *
     H*  FILE USAGE:                                                  *
     H*   SMCOL   PCO01: CO                                   Company *
     H*   WMUPL   PUP01: USER                               User Parm *
     H*   WMUPL1  PUP02: STAFF                              User Parm *
     H*   WMFNL   PFN01: CO/FND                           Fund Master *
     H*   WMREP   PRE01: PHYSICAL                       REGION MASTER *
     H*   WMEMP   PEM01: PHYSICAL                     EMPLOYER MASTER *
     H*****************************************************************
     FW4305F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDSF)
     FWMEMP     IF   E             DISK
     FWMUPL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)
      *  REGION MASTER PYSICAL FILE
     FWMREP     IF   E             DISK
     FWMAAP     IF   E             DISK
      *================================================================

     *** beg chg *** 11/22/13 *****************************************
     D********** STCD            S             10    DIM(12) CTDATA PERRCD(1)   FROM & TO STAFF CODE
     D STCD            S             10    DIM(14) CTDATA PERRCD(1)             FROM & TO STAFF CODE
     *** end chg *** 11/22/13 *****************************************
     D RANGES          S             10    DIM(20) CTDATA PERRCD(1)             SEL. RANGES
     D PCIND           S              2  0 DIM(21) CTDATA PERRCD(21) ASCEND     POSITION CURSOR
     D FND             S             10    DIM(10) CTDATA PERRCD(1) ASCEND      FUND FIELD NAMES
     D PC1IND          S              2  0 DIM(10) CTDATA PERRCD(10) ASCEND     POSITION CURSOR
     D PC2IND          S              2  0 DIM(10) CTDATA PERRCD(10) ASCEND     POSITION CURSOR
     D PC3IND          S              2  0 DIM(10) CTDATA PERRCD(10) ASCEND     POSITION CURSOR
     D FD1             S              3  0 DIM(10)                              FUND ARRAY
     D FD2             S              3    DIM(10)                              FUND ARRAY
     *** beg add *** 11/22/13 *****************************************
     d qqClmCFrom      s              3
     d qqClmConTo      s              3
     *** end add *** 11/22/13 *****************************************
      *
     D ASSN            C                   CONST('ASSIGN STAFF CODES')
     D CHNG            C                   CONST('CHANGE STAFF CODES')
     D*
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D Z1              C                   CONST('00000000000')
     D NINES           C                   CONST('99999999999')
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D                 DS                  INZ
     *** beg chg *** 11/22/13 *****************************************
     D**********  ST                     1     36
     D**********                           DIM(12) ASCEND                       STAFF CODES
     D  ST                     1     42
     D                                     DIM(14) ASCEND                       STAFF CODES
     *** end chg *** 11/22/13 *****************************************
     D  SCFUWR                 1      3
     D  SCTUWR                 4      6
     D  SCFPSV                 7      9
     D  SCTPSV                10     12
     D  SCFPAC                13     15
     D  SCTPAC                16     18
     D  SCFARV                19     21
     D  SCTARV                22     24
     D  SCFAMG                25     27
     D  SCTAMG                28     30
     D  SCFLRP                31     33
     D  SCTLRP                34     36
     *** beg chg *** 11/22/13 *****************************************
     D  scClmCFrom            37     39
     D  scClmConTo            40     42
     *** end chg *** 11/22/13 *****************************************
      *
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS                  INZ
     D  FD                     1     30  0
     D                                     DIM(10)                              FUND ARRAY
     D  SCFD01                 1      3  0
     D  SCFD02                 4      6  0
     D  SCFD03                 7      9  0
     D  SCFD04                10     12  0
     D  SCFD05                13     15  0
     D  SCFD06                16     18  0
     D  SCFD07                19     21  0
     D  SCFD08                22     24  0
     D  SCFD09                25     27  0
     D  SCFD10                28     30  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *   KEY LIST FOR THE COMPANY FILE
      *

     C     COKEY         KLIST
     C                   KFLD                    XXCO#
      *
      *  USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *  FUND MASTER FILE
      *
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND             3 0
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     UPRMPL        PLIST
     C                   PARM      '2'           @@CALL            1
     C                   PARM      *ZEROS        @@RRN             9 0
     C                   PARM      3             @@VIEW            1 0
      *================================================================
      *  LIKE DEFINES
      *
     C     *LIKE         DEFINE    EMCO#         @@CO#
     C     *LIKE         DEFINE    EMFND         @@FND
     C     *LIKE         DEFINE    EMEMP#        @@EMP#
     C     *LIKE         DEFINE    EMDIV         @@DIV
     C     *LIKE         DEFINE    EMCHAP        @@CHAP
      *================================================================
      *  ASSIGN OR CHANGE MODE
      *
     C     QQCALL        COMP      '2'                                    99     =CHANGE STAFF CODES
      *
     C     *IN99         IFEQ      *ON
      *
     C                   MOVEL(P)  CHNG          SCMODE
      *
     C                   ELSE
     C                   MOVEL(P)  ASSN          SCMODE
      *
     C                   ENDIF                                                   *IN99 = ON
      *****************************************************************
      *
      *  DEFAULT "RUN FOR ALL FUNDS" TO "N"
      *
     C                   MOVE      'N'           SCALLF
      *
     C                   Z-ADD     *ZERO         Y1                2 0           INDICATOR INDICES
      *
      *   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
      *
      *  GET THE USER PARAMETER FILE
      *
     C     TOP           TAG
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         XXCO#
     C                   MOVE      *ZERO         XXFND
     C                   MOVE      *BLANK        XXOUTQ
     C                   ELSE
     C                   Z-ADD     UPCO#         XXCO#             3 0
     C                   MOVE      UPFND         XXFND
     C                   MOVE      UPOUTQ        XXOUTQ           10
     C                   END
      *
      *  GET THE COMPANY RECORD
      *
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
     C     FNKEY         CHAIN     PFN01                              60
      *
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
      *
      *   DISPLAY ALL FORMATS
      *
     C     STRT          TAG
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
      *
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
      *
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
      *================================================================
      *   PROCESS ALL COMMAND KEYS
      *
     C     *IN11         IFEQ      *ON
      *================================================================
      *   COMMAND 03 = END OF JOB
      *
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   END
      *================================================================
      *   FUNCTION 12- CANCEL
      *
     C     *INKL         IFEQ      *ON
      *
     C                   SELECT
      *
     C     Q2RECF        WHENEQ    'SCR02'
      *
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C     Q2RECF        WHENEQ    'SCR03'                                      INKL=ON
      *
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  INKL=ON
      *================================================================
      *   COMMAND 10 = SEARCH
      *
     C     *INKJ         IFEQ      *ON
      *
     C                   SELECT
      *
     C     Q2RECF        WHENEQ    'SCR01'
      *
      *   STAFF CODE SEARCH
      *
     C                   Z-ADD     19            Y                 2 0
     C                   Z-ADD     1             X                 3 0
      *
      *  DETERMINE CURSOR LOCATION
      *
     C     HFLD          LOOKUP    STCD(X)                                60
      *
     C     *IN60         IFEQ      *ON
      *
     C                   CALL      'S0005R'      UPRMPL
      *
      *  WAS A STAFF CODE SELECTED. POSITION CURSOR TO CORRECT FIELD
      *
     C     @@RRN         IFNE      *ZEROS
      *
     C     @@RRN         CHAIN     PUP03                              60
      *
     C                   MOVEL     UPSTAF        ST(X)
     C     X             ADD       Y             Z                 3 0
      *
      *  IF ASSIGN MODE SKIP THE "TO" FIELDS ON THE SCREEN
      *
     C     *IN99         IFEQ      *OFF
      *
     C                   ADD       2             Z
      *
     C                   ELSE
      *
     C                   ADD       1             Z
      *
     C                   ENDIF                                                  QQCALL=2    NK
      *
      *  IS THIS THE LAST FIELD ON THE SCREEN
      *
      ******* beg chg ****** 11/22/13 ********************************
     C*******     Z             IFGT      31
      /free
       If z > 33;
      /end-free
      ******* end chg ****** 11/22/13 ********************************
     C                   MOVE      20            Z
     C                   ENDIF                                                  QQSTAF NE   NK
      *
      ******* beg add ******** 11/22/13 ******************************
      /free
       focFrClaim = *off;
       focToClaim = *off;
       If z = 32;
          focFrClaim = *on;
       ElseIf z = 33;
          focToClaim = *on;
       Else;
      /end-free
      ******* end add ******** 11/22/13 ******************************
     C                   MOVE      *ON           *IN(Z)
      ******* beg add ******** 11/22/13 ******************************
      /free
       Endif;
      /end-free
      ******* end add ******** 11/22/13 ******************************
      *
      *  F12 OR ENTER WERE PRESSED POSITION TO THE CORRECT FIELD
      *
     C                   ELSE
     C     Y             ADD       X             Z
      ******* beg add ******** 11/22/13 ******************************
      /free
       focFrClaim = *off;
       focToClaim = *off;
       If z = 32;
          focFrClaim = *on;
       ElseIf z = 33;
          focToClaim = *on;
       Else;
      /end-free
      ******* end add ******** 11/22/13 ******************************
     C                   MOVE      *ON           *IN(Z)
      ******* beg add ******** 11/22/13 ******************************
      /free
       Endif;
      /end-free
      ******* end add ******** 11/22/13 ******************************
      *
     C                   ENDIF                                                  *INKL = OFF
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *IN60= *ON
      *
     C     Q2RECF        WHENEQ    'SCR02'
      *================================================================
      *  SCREEN 2 SEARCHES
      *================================================================
      *  EMPLOYER NUMBER SEARCH
      *
     C     HFLD          IFEQ      'SCFREN'
     C     HFLD          OREQ      'SCTOEN'
     C     HFLD          OREQ      'SCFREA'
     C     HFLD          OREQ      'SCTOEA'
     **
     C     HFLD          IFEQ      'SCFREN'
     C     HFLD          OREQ      'SCTOEN'
      *
     C                   MOVE      2             XXVIEW            1 0
      *
     C                   ELSE
     C                   MOVE      1             XXVIEW
      *
     C                   ENDIF                                                  HFLD=SCFREN
      *
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM      0             @@RRN             9 0
     C                   PARM      DBRRNF        @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      XXVIEW        @@VIEW            1 0
      *
      *  A SELECTION WAS MADE
      *
     C     @@RRN         IFNE      *ZERO
      *
     C     @@RRN         CHAIN     WMEMP                              60
      *
     C     *IN60         IFEQ      *OFF
      *
      *  LOAD THE VALUE EITHER EMP NAME OR EMP NUMBER
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCFREN'
      *
     C                   MOVE      EMEMP#        SCFREN
      *
     C     HFLD          WHENEQ    'SCTOEN'
      *
     C                   MOVE      EMEMP#        SCTOEN
      *
     C     HFLD          WHENEQ    'SCFREA'
      *
     C                   MOVEL     EMNAME        SCFREA
      *
     C     HFLD          WHENEQ    'SCTOEA'
      *
     C                   MOVEL     EMNAME        SCTOEA
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  *IN60=OFF
      *
     C                   ENDIF                                                  @@RRN NE 0
      *
     C                   Z-ADD     1             X
      *
      *  POSITION THE CURSOR
      *
     C     HFLD          LOOKUP    RANGES(X)                              60
      *
     C                   MOVE      PCIND(X)      Y
      *
     C     @@RRN         IFNE      *ZERO
     C                   ADD       1             Y
     C                   ENDIF                                                  @@RRN NE 0
      *
     C                   MOVE      *ON           *IN(Y)
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD=SCFREN
      *================================================================
      *  TABLE SEARCH- CHAPTER
      *
     C     HFLD          IFEQ      'SCFCHP'
     C     HFLD          OREQ      'SCTCHP'
      *
     C                   CALL      'W0720R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRNF        @@RRN2            9 0
     C                   PARM      *ZEROS        @@CHAP
      *
      *  A SELECTION WAS MADE.  WHICH FIELD?
      *
     C     @@CHAP        IFNE      *ZEROS
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCFCHP'
      *
     C                   MOVE      @@CHAP        SCFCHP
      *
     C     HFLD          WHENEQ    'SCTCHP'
      *
     C                   MOVE      @@CHAP        SCTCHP
      *
     C                   ENDSL                                                  @@CHAP<>*ZEROS
      *
     C                   ENDIF                                                  @@CHAP<>*ZEROS
      *
      *  POSITION THE CURSOR TO THE CORRECT FIELD
      *
     C                   Z-ADD     1             X
      *
     C     HFLD          LOOKUP    RANGES(X)                              60
      *
     C     @@CHAP        IFNE      *ZERO
      *
     C                   ADD       1             X
      *
     C                   ENDIF                                                  @@CHAP NE 0
      *
     C                   MOVE      PCIND(X)      Y
     C                   MOVE      *ON           *IN(Y)
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD=SCFCHP OS
      *================================================================
      *  TABLE SEARCH- REGION
      *
     C     HFLD          IFEQ      'SCFRGN'
     C     HFLD          OREQ      'SCTRGN'
      *
     C                   CALL      'W0520R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM      *ZEROS        @@RRN             9 0
     C                   PARM      DBRRNF        @@RRN2            9 0
      *
      *  VALUE SELECTED. CHAIN TO THE REGION PHYSICAL FILE
      *
     C     @@RRN         IFNE      *ZEROS
      *
     C     @@RRN         CHAIN     WMREP                              60
      *
     C     *IN60         IFEQ      *OFF
      *
      *  WHICH FIELD WAS SELECTED
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCFRGN'
      *
     C                   MOVE      RERGN#        SCFRGN
      *
     C     HFLD          WHENEQ    'SCTRGN'
      *
     C                   MOVE      RERGN#        SCTRGN
      *
     C                   ENDSL                                                  HFLD =SCFRGNOS
      *
     C                   ENDIF                                                  *IN60=OFF   OS
      *
     C                   ENDIF                                                  @@RRN NE 0  OS
      *
      *  POSITION THE CURSOR TO THE CORRECT FIELD
      *
     C                   Z-ADD     1             X
      *
     C     HFLD          LOOKUP    RANGES(X)                              60
      *
     C     @@RRN         IFNE      *ZERO
      *
     C                   ADD       1             X
      *
     C                   ENDIF                                                  @@RRN NE 0
      *
     C                   MOVE      PCIND(X)      Y
     C                   MOVE      *ON           *IN(Y)
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD=SCFRGN OS
      *================================================================
      *  TABLE SEARCH- COUNTY CODE
      *
     C     HFLD          IFEQ      'SCFCTY'
     C     HFLD          OREQ      'SCTCTY'
      *
     C                   CALL      'W2215R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@MODE            1
     C                   PARM      *ZERO         @@RRN             9 0
     C                   PARM      *BLANKS       @@KEY            10
     C                   PARM      *BLANKS       @@ST              2
      *
     C     @@KEY         IFNE      *BLANKS
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCFCTY'
      *
     C                   MOVEL     @@KEY         SCFCTY
      *
     C     HFLD          WHENEQ    'SCTCTY'
      *
     C                   MOVEL     @@KEY         SCTCTY
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  @@KEY NE BLANKS
      *
      *  POSITION THE CURSOR TO THE CORRECT FIELD
      *
     C                   Z-ADD     1             X
      *
     C     HFLD          LOOKUP    RANGES(X)                              60
      *
     C     @@KEY         IFNE      *BLANK
      *
     C                   ADD       1             X
      *
     C                   ENDIF                                                  @@KEY NE BLANKS
      *
     C                   MOVE      PCIND(X)      Y
     C                   MOVE      *ON           *IN(Y)
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD=SCFCTY
      *================================================================
      *  TABLE SEARCH - AGENT NUMBER
      *
     C     HFLD          IFEQ      'SCFRAN'
     C     HFLD          OREQ      'SCTOAN'
     C     HFLD          OREQ      'SCFRAA'
     C     HFLD          OREQ      'SCTOAA'
     **
     C                   CALL      'W0040R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      DBRRNF        ##RRN             9 0
     C                   PARM      2             ##VIEW            1 0
     **
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
      *
      *  A SELECTION WAS MADE.  FIND OUT WHICH FIELD IT IS
      *
     C     ##RRN         IFNE      *ZEROS
      *
     C     ##RRN         CHAIN     PAA01                              99
      *
     C                   MOVE      AAAGT#        @@AGT#
      *
     C                   SELECT
     **
     C     HFLD          WHENEQ    'SCFRAN'
     C                   MOVE      AAAGT#        SCFRAN
     **
     C     HFLD          WHENEQ    'SCTOAN'
     C                   MOVE      AAAGT#        SCTOAN
     **
     C     HFLD          WHENEQ    'SCTOAN'
     C                   MOVE      AAAGT#        SCTOAN
     **
     C     HFLD          WHENEQ    'SCFRAA'
     C     HFLD          OREQ      'SCTOAA'
     **
     C                   CALL      'WA002R'
     C                   PARM      XXCO#         @@CO#
     C                   PARM      XXFND         @@FND
     C                   PARM      *ZERO         @@EMP#
     C                   PARM      *ZERO         @@DIV
     C                   PARM      *ZERO         @@FYR             3 0
     C                   PARM                    @@AGT#            5 0
     C                   PARM      *BLANKS       @@ANAM           40
      *
      *  A SELECTION WAS MADE.  FIND OUT WHICH FIELD IT IS
      *
     C     @@ANAM        IFNE      *BLANKS
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCFRAA'
      *
     C                   MOVEL     @@ANAM        SCFRAA
      *
     C     HFLD          WHENEQ    'SCTOAA'
      *
     C                   MOVEL     @@ANAM        SCTOAA
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  @@ANAM NE BLANK
      *
     C                   ENDSL                                                  HFLD = SCFRAA
      *
     C                   ENDIF                                                  @@AGT# NE 0
      *
      *  POSITION THE CURSOR TO THE CORRECT FIELD
      *
     C                   Z-ADD     1             X
      *
     C     HFLD          LOOKUP    RANGES(X)                              60
      *
     C     @@AGT#        IFNE      *ZERO
      *
     C                   ADD       1             X
      *
     C                   ENDIF                                                  @@AGT# NE 0
      *
      *  SET THE APPROPRIATE INDICATOR
      *
     C                   MOVE      PCIND(X)      Y
     C                   MOVE      *ON           *IN(Y)
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD=SCFRAN
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCACST'
      *
     C                   MOVE      *ON           *IN40
      *
     C     HFLD          WHENEQ    'SCFEZP'
      *
     C                   MOVE      *ON           *IN41
      *
     C     HFLD          WHENEQ    'SCTEZP'
      *
     C                   MOVE      *ON           *IN42
      *
     C     HFLD          WHENEQ    'SCFANP'
      *
     C                   MOVE      *ON           *IN43
      *
     C     HFLD          WHENEQ    'SCTANP'
      *
     C                   MOVE      *ON           *IN44
      *
     C     HFLD          WHENEQ    'SCFAZP'
      *
     C                   MOVE      *ON           *IN45
      *
     C     HFLD          WHENEQ    'SCTAZP'
      *
     C                   MOVE      *ON           *IN46
     C                   GOTO      STRT
      *
     C     HFLD          WHENEQ    'SCALLF'
      *
     C                   MOVE      *ON           *IN70
     C                   GOTO      STRT
      *
     C                   ENDSL
      *
     C     Q2RECF        WHENEQ    'SCR03'
      *
      *****************************************************************
      *  FUND SEARCH
      *****************************************************************
     C                   Z-ADD     1             X
      *
     C     HFLD          LOOKUP    FND(X)                                 60
      *
     C     *IN60         IFEQ      *ON
      *
     C                   MOVE      XXCO#         ##CO#             3
      *
     C                   CALL      'W0010R'
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM      '2'           @@CALL
     C                   PARM                    ##CO#
     C                   PARM      *BLANKS       ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM      1             @@VIEW
      *
      *  POSITION THE CURSOR
      *
     C     ##FND         IFNE      *BLANKS
      *
     C                   MOVE      ##FND         FD(X)
      *
      *  IS IT THE LAST FIELD ON THE SCREEN
      *
     C     X             IFEQ      10
      *
     C                   Z-ADD     1             X
      *
     C                   ELSE
     C                   ADD       1             X
      *
     C                   ENDIF                                                  X=10
      *
     C                   ENDIF                                                  @@FND NE 0
      *
     C                   MOVE      PC1IND(X)     Y
      *
     C                   MOVE      *ON           *IN(Y)
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  IN60=ON
      *
     C                   ENDSL                                                  Q2RECF=SCR01
      *
     C                   GOTO      STRT
      *
     C                   END                                                    *INKJ=*ON
      *================================================================
      *   PROCESS HELP REQUESTS
      *
     C     *IN10         IFEQ      *ON
      *
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE      XXRECF        Q2RECF
      *
     C                   GOTO      STRT
      *
     C                   END                                                    IN10=ON
      *================================================================
      *   PROCESS CHANGE OF USER PARAMETERS
      *
     C     *INKM         IFEQ      *ON
      *
     C                   CALL      'W0006R'
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKY          CHAIN     PUP01                              60
      *
     C     *IN60         IFEQ      '1'
      *
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
      *
     C                   END
      *
     C                   GOTO      TOP
      *
     C                   END                                                    INKM=ON
      *
     C                   END                                                    IN11=ON
      *
      *================= END OF FUNCTION KEY PROCESSING ===============
      *
      *   GOTO THE APPROPRIATE SCREEN
      *
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
      *
      *   PROCESS REQUEST SCREEN
      *
     C     SCR01T        TAG
      *
      *  EACH STAFF CODE MUST BE VALID IF ENTERED
      *
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     1             X1                2 0
      *
     C                   ELSE
     C                   Z-ADD     2             X1
      *
     C                   ENDIF                                                  *IN99=ON
      *
     C                   MOVE      *OFF          XXFLAG            1
     C                   Z-ADD     1             X
      *
      *  CHECK EACH STAFF CODE IF ENTERED
      *
      ****** beg chg ******** 11/22/13 **********************************
     C*****X             DOWLE     12
      /free
       Dow x <= 14;
      /end-free
      ****** end chg ******** 11/22/13 **********************************
      *
     C     ST(X)         IFNE      *BLANKS
      *
     C                   MOVE      *ON           XXFLAG                         ENTRY WAS MADE ODE
      *
      *  "FROM" STAFF CODE DOES NOT HAVE TO BE VALID UNLESS ASSIGN MODE
      *
     C                   Z-ADD     *ZERO         R1                2 0                      F CODE
      *
     C     *IN99         IFEQ      *ON
      *
     C     X             DIV       2             TEMP              2 0                      F CODE
     C                   MVR                     R1                                         F CODE
      *
     C                   ENDIF                                                  *IN99=OFF
      *
     C     R1            IFEQ      *ZERO
      *
     C     ST(X)         CHAIN     PUP02                              60
      *
     C                   ELSE
      *
     C                   MOVE      *OFF          *IN60
      *
     C                   ENDIF                                                  R1=0
      *
      *  INVALID STAFF CODE
      *
     C     *IN60         IFEQ      *ON
      *
     C     X             ADD       31            Y1
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *IN60=ON
      *
     C                   ELSE
      *
      *  VALID CODE. CHECK BOTH "FROM" AND "TO" VALUES WERE ENTERED
      *
     C     *IN99         IFEQ      *ON
      *
      *  IS THIS THE "FROM" OR THE "TO" FIELD
      *
     C                   Z-ADD     *ZERO         R1                                         F CODE
     C     X             DIV       2             TEMP                                       F CODE
     C                   MVR                     R1                                         F CODE
      *
     C     R1            IFEQ      *ZERO
      *
     C     X             SUB       1             X2                2 0                      F CODE
      *
     C     ST(X2)        IFNE      *BLANKS
      *
      ****** beg chg ******** 11/22/13 ********************************
     C******             ADD       44            X2
      /free
       x2 = x2 + 46;
      /end-free
      ****** end chg ******** 11/22/13 ********************************
     C                   MOVE      *ON           *IN(X2)
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  ST,X2=BLANKS
      *
      *
     C                   ELSE
      *
     C     X             ADD       1             X2                2 0                      F CODE
      *
      *
     C                   ENDIF                                                  R1=0
      *
      *  IF ONE OR THE OTHER ISNT BLANK SEND THE USER AN ERROR MESSAGE
      *
     C                   ENDIF                                                  *IN99=ON
      *
     C                   ENDIF                                                  CD,X NE BLANKS
      *
     C                   ADD       X1            X
      *
     C                   ENDDO                                                  X DOWLE 12
      *
      *  NO STAFF CODES WERE ENTERED. SEND USER ERROR MESSAGE
      *
     C     XXFLAG        IFEQ      *OFF
      *
      ******* beg chg ******** 11/22/13 ***************************
     C*******            MOVE      *ON           *IN56
      /free
       *in61 = *on;
      /end-free
      ******* end chg ******** 11/22/13 ***************************
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *IN56=ON
      *
      *  ALL EDITS PASSED FOR SCREEN 1. GO TO SCREEN 2
      *
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
      *
      *   PROCESS REQUEST SCREEN
      *
     C     SCR02T        TAG
      *
      *   ACCOUNT STATUS MUST BE 'A', 'B', OR 'C'
      *
     C     SCACST        IFNE      'A'
     C     SCACST        ANDNE     'C'
     C     SCACST        ANDNE     'B'
     C                   MOVE      *ON           *IN47
     C                   GOTO      STRT
     C                   ENDIF
     ******************************************************************
     **
     **  VALIDATE THE EMPLOYER SORT RANGES
     **
     C     SCTOEA        IFNE      *BLANK
     **
     **    CONVERT TO UPPER CASE
     **
     C     LO:UP         XLATE(P)  SCFREA        SCFREA
     C     LO:UP         XLATE(P)  SCTOEA        SCTOEA
     **
     C     SCTOEN        IFNE      *ZERO
     C                   MOVE      *ON           *IN48
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
      * VALIDATE TO RANGE NOT LESS THAN FROM RANGE
      *
     C     SCFREA        IFGT      SCTOEA
     C                   MOVE      *ON           *IN49
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFREA IFGT
      *
     C     SCFREN        IFGT      SCTOEN
     C                   MOVE      *ON           *IN50
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFREN IFGT
      *
      *  VALIDATE FROM NOT GREATER THAN TO CHAPTER
      *
     C     SCFCHP        IFGT      SCTCHP
     C                   MOVE      *ON           *IN51
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFCHP IFGT
      *
      *  VALIDATE FROM NOT GREATER THAN TO REGION
      *
     C     SCFRGN        IFGT      SCTRGN
     C                   MOVE      *ON           *IN52
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFRGN IFGT
      *
      *  VALIDATE FROM NOT GREATER THAN TO ZIPCODE
      *
     C     SCFEZP        IFGT      SCTEZP
     C                   MOVE      *ON           *IN53
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFEZP IFGT
      *
      *  VALIDATE FROM NOT GREATER THAN TO COUNTY
      *
     C     SCFCTY        IFGT      SCTCTY
     C                   MOVE      *ON           *IN54
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFCTY IFGT
      *
      *  VALIDATE FROM NOT GREATER THAN TO ANNUAL PREMIUM
      *
     C     SCFANP        IFGT      SCTANP
     C                   MOVE      *ON           *IN55
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFANP IFGT
      *
      *  VALIDATE THE AGENT SORT RANGES
      *
     C     SCTOAA        IFNE      *BLANK
      *
      *    CONVERT TO UPPER CASE
      *
     C     LO:UP         XLATE(P)  SCFRAA        SCFRAA
     C     LO:UP         XLATE(P)  SCTOAA        SCTOAA
      *
     C     SCTOAN        IFNE      *ZERO
     C                   MOVE      *ON           *IN56
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   ENDIF                                                  SCTOAA IFNE
      *
     C     SCFRAA        IFGT      SCTOAA
     C                   MOVE      *ON           *IN57
     C                   GOTO      STRT
     C                   ENDIF                                                   SCFRAA IFGT
      *
     C     SCFRAN        IFGT      SCTOAN
     C                   MOVE      *ON           *IN58
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFRAA IFGT
      *
      *  VALIDATE FROM NOT GREATER THAN TO AGENT ZIP CODE
      *
     C     SCFAZP        IFGT      SCTAZP
     C                   MOVE      *ON           *IN59
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFAZP IFGT
      *
      *  VALIDATE AT LEAST ONE RANGE WAS ENTERED
      *
     C     SCTOEN        IFEQ      *ZERO
     C     SCTOEA        ANDEQ     *BLANKS
     C     SCTCHP        ANDEQ     *ZERO
     C     SCTRGN        ANDEQ     *ZERO
     C     SCTEZP        ANDEQ     *ZERO
     C     SCTCTY        ANDEQ     *BLANKS
     C     SCTANP        ANDEQ     *ZERO
     C     SCTOAA        ANDEQ     *BLANKS
     C     SCTOAN        ANDEQ     *ZERO
     C     SCTAZP        ANDEQ     *ZERO
      *
     C                   MOVE      *ON           *IN72
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCTOEN=0
      *================================================================
      *  PROCESS FOR ONE OR MORE FUNDS
      *
     C                   SELECT
      *
     C     SCALLF        WHENEQ    'N'
      *
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C                   GOTO      STRT
      *
     C     SCALLF        WHENEQ    'Y'
      *
     C                   GOTO      CALLIT
      *
     C                   OTHER
      *
      *  INVALID ENTRY. MUST BE 'Y' OR 'N'.
      *
     C                   MOVE      *ON           *IN71
     C                   GOTO      STRT
      *
     C                   ENDSL
      *
      *================================================================
      *  VALIDATE FUNDS
      *
     C     SCR03T        TAG
      *
     C                   MOVE      *OFF          *IN78
      *
     C     1             DO        10            X
      *
     C     FD(X)         IFNE      *ZEROS
      *
     C                   MOVE      FD(X)         XXFND
      *
     C     FNKEY         CHAIN     PFN01                              60
      *
      *  INVALID FUND ENTERED
      *
     C     *IN60         IFEQ      *ON
      *
     C                   MOVE      PC2IND(X)     Y
     C                   MOVE      *ON           *IN(Y)
     C                   GOTO      STRT
      *
     C                   ELSE                                                   *IN60=ON
     C                   MOVE      *ON           *IN78
      *
     C                   ENDIF                                                  *IN60=ON
      *
     C                   ENDIF                                                  FD,X NE BLAN
      *
     C                   ENDDO
      *
     C     *IN78         IFEQ      *OFF
      *
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   MOVE      *ON           *IN61
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
      *  CHECK FOR DUPLICATE FUNDS ENTERED
      *
     C                   DO        10            X
      *
     C     X             ADD       1             Y
      *
     C     Y             DO        10            Z
      *
     C     FD(X)         IFEQ      FD(Z)
     C     FD(X)         ANDNE     *ZERO
      *
     C                   MOVE      PC3IND(Z)     Y
     C                   MOVE      *ON           *IN(Y)
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  FD,X = FD,Z
      *
     C                   ENDDO                                                  Y DO 10
      *
     C                   ENDDO                                                  X DO 10
      *================================================================
      *  ALL EDITS PASSED, LOAD PARMS FOR CALL TO CL PROGRAM
      *
     C     CALLIT        TAG
      *
     C                   MOVE      XXCO#         QQCO#             3
      *
      *  LOAD UNDERWRITER IF NOT BLANK
      *
     C     SCFUWR        IFNE      *BLANK
      *
     C                   MOVE      SCFUWR        QQFUWR            3
     ***** Beg Add **** 12/23/11 **************************************
     C                   ENDIF                                                  SCFUWR NE
     ***** End Add **** 12/23/11 **************************************
      *
     C     SCTUWR        IFNE      *BLANK
      *
     C                   MOVE      SCTUWR        QQTUWR            3
      *
     C                   ENDIF                                                  SCTUWR NE
      *
     ***** Beg Del **** 12/23/11 **************************************
     C******               ENDIF                           SCFUWR NE
     ***** End Del **** 12/23/11 **************************************
      *
      *  LOAD POLICY SERVICE REP IF NOT BLANK
      *
     C     SCFPSV        IFNE      *BLANK
      *
     C                   MOVE      SCFPSV        QQFPSV            3
     ***** Beg Add **** 12/23/11 **************************************
     C                   ENDIF                                                  SCFPSV NE
     ***** End Add **** 12/23/11 **************************************
      *
     C     SCTPSV        IFNE      *BLANK
      *
     C                   MOVE      SCTPSV        QQTPSV            3
      *
     C                   ENDIF                                                  SCTPSV NE
      *
     ***** Beg Del **** 12/23/11 **************************************
     C*******              ENDIF                           SCFPSV NE
     ***** End Del **** 12/23/11 **************************************
      *
      *  LOAD PREMIUM ACCOUNT REP IF NOT BLANK
      *
     C     SCFPAC        IFNE      *BLANK
      *
     C                   MOVE      SCFPAC        QQFPAC            3
     ***** Beg Add **** 12/23/11 **************************************
     C                   ENDIF                                                  SCFPAC NE
     ***** End Add **** 12/23/11 **************************************
      *
     C     SCTPAC        IFNE      *BLANK
      *
     C                   MOVE      SCTPAC        QQTPAC            3
      *
     C                   ENDIF                                                  SCTPAC NE
      *
     ***** Beg Del **** 12/23/11 **************************************
     C*******              ENDIF                           SCFPAC NE
     ***** End Del **** 12/23/11 **************************************
      *
      *  LOAD AUDIT REVIEWER IF NOT BLANK
      *
     C     SCFARV        IFNE      *BLANK
      *
     C                   MOVE      SCFARV        QQFARV            3
     ***** Beg Add **** 12/23/11 **************************************
     C                   ENDIF                                                  SCFARV NE
     ***** End Add **** 12/23/11 **************************************
      *
     C     SCTARV        IFNE      *BLANK
      *
     C                   MOVE      SCTARV        QQTARV            3
      *
     C                   ENDIF                                                  SCTARV NE
      *
     ***** Beg Del **** 12/23/11 **************************************
     C*********            ENDIF                           SCFARV NE
     ***** End Del **** 12/23/11 **************************************
      *
      *  LOAD AUDIT MANAGER IF NOT BLANK
      *
     C     SCFAMG        IFNE      *BLANK
      *
     C                   MOVE      SCFAMG        QQFAMG            3
     ***** Beg Add **** 12/23/11 **************************************
     C                   ENDIF                                                  SCFAMG NE
     ***** End Add **** 12/23/11 **************************************
      *
     C     SCTAMG        IFNE      *BLANK
      *
     C                   MOVE      SCTAMG        QQTAMG            3
      *
     C                   ENDIF                                                  SCTAMG NE
      *
     ***** Beg Del **** 12/23/11 **************************************
     C********             ENDIF                           SCFAMG NE
     ***** End Del **** 12/23/11 **************************************
      *
      *  LOAD LOSS CONTROL REP IF NOT BLANK
      *
     C     SCFLRP        IFNE      *BLANK
      *
     C                   MOVE      SCFLRP        QQFLRP            3
     ***** Beg Add **** 12/23/11 **************************************
     C                   ENDIF                                                  SCFLRP NE
     ***** End Add **** 12/23/11 **************************************
      *
     C     SCFLRP        IFNE      *BLANK
     C                   MOVE      SCTLRP        QQTLRP            3
      *
     C                   ENDIF                                                  SCTLRP NE
      *
     ***** Beg Del **** 12/23/11 **************************************
     C******               ENDIF                           SCFLRP NE
     ***** End Del **** 12/23/11 **************************************
     *** beg add *** 11/22/13 *****************************************
      *  Load Claim Contact
      /free
       If scClmCFrom <> ' ';
          qqClmCFrom =  scClmCFrom;
       Endif;

       If scClmConTo <> ' ';
          qqClmConTo = scClmConTo;
       Endif;
      /end-free
     *** end add *** 11/22/13 *****************************************
      *
      *  LOAD ACCOUNT STATUS
      *
     C                   MOVEL     SCACST        QQACST            1
      *
      *  LOAD EMPLOYER NUMBER IF NOT BLANK
      *
     C     SCTOEN        IFNE      *ZERO
      *
     C                   MOVE      SCFREN        QQFREN            9
     C                   MOVE      SCTOEN        QQTOEN            9
      *
     C                   ELSE
      *
     C                   MOVE      Z1            QQFREN
     C                   MOVE      NINES         QQTOEN
      *
     C                   ENDIF                                                  SCTOEN NE 0
      *
      *  LOAD EMPLOYER NAME IF NOT BLANK
      *
     C     SCTOEA        IFNE      *BLANK
      *
     C                   MOVE      SCFREA        QQFREA            2
     C                   MOVE      SCTOEA        QQTOEA            2
      *
     C                   ELSE
      *
     C                   MOVE      *BLANKS       QQFREA
     C                   MOVE      'ZZ'          QQTOEA
      *
     C                   ENDIF                                                  SCTOEA NE
      *
      *  LOAD CHAPTER IF NOT BLANK
      *
     C     SCTCHP        IFNE      *ZERO
      *
     C                   MOVE      SCFCHP        QQFCHP            3
     C                   MOVE      SCTCHP        QQTCHP            3
      *
     C                   ELSE
      *
     C                   MOVE      '000'         QQFCHP
     C                   MOVE      '999'         QQTCHP
      *
     C                   ENDIF                                                  SCTCHP NE
      *
      *  LOAD REGION IF NOT BLANK
      *
     C     SCTRGN        IFNE      *ZERO
      *
     C                   MOVE      SCFRGN        QQFRGN            3
     C                   MOVE      SCTRGN        QQTRGN            3
      *
     C                   ELSE
      *
     C                   MOVE      '000'         QQFRGN
     C                   MOVE      '999'         QQTRGN
      *
     C                   ENDIF                                                  SCTRGN NE
      *
      *  LOAD EMPLOYER ZIP CODE IF NOT BLANK
      *
     C     SCTEZP        IFNE      *ZERO
      *
     C                   MOVEL     SCFEZP        QQFEZP            9
     C                   CAT       '0000':0      QQFEZP            9
     C                   MOVEL     SCTEZP        QQTEZP            9
     C                   CAT       '9999':0      QQTEZP            9
      *
     C                   ELSE
      *
     C                   MOVE      Z1            QQFEZP
     C                   MOVE      NINES         QQTEZP
      *
     C                   ENDIF                                                  SCTEZP NE
      *
      *  LOAD COUNTY IF NOT BLANK
      *
     C     SCTCTY        IFNE      *BLANK
      *
     C                   MOVE      SCFCTY        QQFCTY            4
     C                   MOVE      SCTCTY        QQTCTY            4
      *
     C                   ELSE
      *
     C                   MOVE      *BLANKS       QQFCTY
     C                   MOVE      'ZZ99'        QQTCTY
      *
     C                   ENDIF                                                  SCTCTY NE
      *
      *  LOAD ANNUAL PREMIUM IF NOT BLANK
      *
     C     SCTANP        IFNE      *ZERO
      *
     C                   MOVE      SCFANP        QQFANP           11
     C                   MOVE      SCTANP        QQTANP           11
      *
     C                   ELSE
      *
     C                   MOVE      Z1            QQFANP
     C                   MOVE      NINES         QQTANP
      *
     C                   ENDIF                                                  SCTANP NE
      *
      *  LOAD AGENT NAME IF NOT BLANK
      *
     C     SCTOAA        IFNE      *BLANK
      *
     C                   MOVE      SCFRAA        QQFRAA            2
     C                   MOVE      SCTOAA        QQTOAA            2
      *
     C                   ELSE
      *
     C                   MOVE      *BLANKS       QQFRAA
     C                   MOVE      'ZZ'          QQTOAA
      *
     C                   ENDIF                                                  SCTOAA NE
      *
      *  LOAD AGENT NUMBER IF NOT BLANK
      *
     C     SCTOAN        IFNE      *ZERO
      *
     C                   MOVE      SCFRAN        QQFRAN            5
     C                   MOVE      SCTOAN        QQTOAN            5
      *
     C                   ELSE
      *
     C                   MOVE      Z1            QQFRAN
     C                   MOVE      NINES         QQTOAN
      *
     C                   ENDIF                                                  SCTOAN NE
      *
      *  LOAD AGENT ZIP CODE IF NOT ZERO
      *
     C     SCTAZP        IFNE      *ZERO
      *
     C                   MOVEL     SCFAZP        QQFAZP            9
     C                   CAT       '0000':0      QQFAZP            9
     C                   MOVEL     SCTAZP        QQTAZP            9
     C                   CAT       '9999':0      QQTAZP            9
      *
     C                   ELSE
      *
     C                   MOVE      Z1            QQFAZP
     C                   MOVE      NINES         QQTAZP
      *
     C                   ENDIF                                                  SCTAZP NE
      *
     C                   MOVEL     XXOUTQ        QQOUTQ           10
     C                   MOVE      QQCALL        QQCHG             1
      *
      *  SORT THE FUND ARRAY
      *
     C                   SORTA     FD
      *
      *  MOVE BLANKS ARRAY ELEMENTS TO THE END OF THE ARRAY
      *
     C     SCALLF        IFEQ      'N'
      *
     C                   MOVE      *ZEROS        FD1
      *
     C                   Z-ADD     1             X
      *
     C     FD(X)         DOUNE     *ZERO
     C     X             OREQ      10
      *
     C                   ADD       1             X
      *
     C                   ENDDO                                                  FD,X DOUNE
      *
      *  LOAD THE NEW ARRAY
      *
     C                   MOVEA     FD(X)         FD1
     C                   MOVE      FD1           FD2
      *
     C                   ENDIF                                                  FD NE 0
      *
      *  SUBMIT THE CL PROGRAM
      *
     C                   CALL      'W4305C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFUWR
     C                   PARM                    QQTUWR
     C                   PARM                    QQFPSV
     C                   PARM                    QQTPSV
     C                   PARM                    QQFPAC
     C                   PARM                    QQTPAC
     C                   PARM                    QQFARV
     C                   PARM                    QQTARV
     C                   PARM                    QQFAMG
     C                   PARM                    QQTAMG
     C                   PARM                    QQFLRP
     C                   PARM                    QQTLRP
     C                   PARM                    QQACST
     C                   PARM                    QQFREN
     C                   PARM                    QQTOEN
     C                   PARM                    QQFREA
     C                   PARM                    QQTOEA
     C                   PARM                    QQFCHP
     C                   PARM                    QQTCHP
     C                   PARM                    QQFRGN
     C                   PARM                    QQTRGN
     C                   PARM                    QQFEZP
     C                   PARM                    QQTEZP
     C                   PARM                    QQFCTY
     C                   PARM                    QQTCTY
     C                   PARM                    QQFANP
     C                   PARM                    QQTANP
     C                   PARM                    QQFRAA
     C                   PARM                    QQTOAA
     C                   PARM                    QQFRAN
     C                   PARM                    QQTOAN
     C                   PARM                    QQFAZP
     C                   PARM                    QQTAZP
     C                   PARM                    QQOUTQ
     C                   PARM      'D'           QQMODE            1
     C                   PARM                    QQCHG
     C                   PARM                    FD2
     *** beg add *** 11/22/13 *****************************************
     C                   PARM                    qqClmCFrom
     C                   PARM                    qqClmConTo
     *** end add *** 11/22/13 *****************************************
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**  STCD
SCFUWR
SCTUWR
SCFPSV
SCTPSV
SCFPAC
SCTPAC
SCFARV
SCTARV
SCFAMG
SCTAMG
SCFLRP
SCTLRP
SCCLMCFROM
SCCLMCONTO
**  RANGES
SCFREN
SCTOEN
SCFREA
SCTOEA
SCFCHP
SCTCHP
SCFRGN
SCTRGN
SCFEZP
SCTEZP
SCFCTY
SCTCTY
SCFANP
SCTANP
SCFRAA
SCTOAA
SCFRAN
SCTOAN
SCFAZP
SCTAZP
** PCIND
202122232425262728293031323334353637383961
** FND
SCFD01
SCFD02
SCFD03
SCFD04
SCFD05
SCFD06
SCFD07
SCFD08
SCFD09
SCFD10
** PC1IND
20212223242526272829
** PC2IND
30313233343536373839
** PC3IND
40414243444546474849
