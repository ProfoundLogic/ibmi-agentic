     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W0006R - USER PARAMETER FILE MAINTENANCE                 *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                              *
     H* 11/06/23  C:333 Project: WC2926                              *
     H*                 Policy copy and search enhancements          *
     H*                 DSPF changes :                               *
     H*                  W0006F : Adding new drop down and buttons.  *
     H*                                                              *
     H* 08/21/23  C:079 Support: 110458-133                          *
     H*                 Write to SMUPP if MC installed instead of if *
     H*                 UPsub# <> 0                                  *
     H*                                                              *
     H* 03/08/22  C:300 Support: 101176 (UBIC)                       *
     H*                 Change USER Parms not showing Group Number.  *
     H*                 Chain to SMUPP before WMUPP.                 *
     H*                                                              *
     H* 11/22/17  C:33  Support: 50697                               *
     H*                 Only update SMUPP/UPFND when UPSUB# exists.  *
     H*                                                              *
     H* 03/01/16  C:65  Project: SY251                               *
     H*                 Add network printer dropdown for selection   *
     H*                 of default printer for scriptura enabled     *
     H*                 processes.                                   *
     H*                                                               *
     H* 06/10/14 C:279 114WNK                                         *
     H*                Add UPMXRI field.                              *
     H*                                                               *
     H* 04/19/06 C:58  WC1383                                         *
     H*                Add subset W/ staff code flag                  *
     H*                                                               *
     H* 05/30/03 C:132 SUPPORT CI091 37370                            *
     H*                Load WMUPP/UPSTAT with an "A" on Add/Update.   *
     H*                                                               *
     H* 05/13/02 C:61  SUPPORT WA099 31782                            *
     H*                Correct issues with searches.                  *
     H*                                                               *
     H*  05/16/00 C:33 SEQ#: OP082/21740                              *
     H*                DO NOT RESTRICT DIVISION IF EMPLOYER IS NOT    *
     H*                RESTRICTED                                     *
     H*                                                               *
     H* 11/30/99 C:118 PROJECT: WC577                                 *
     H*                Change name of file displayed, and other V3    *
     H*                changes.                                       *
     H*                                                               *
     H* 06/02/99 C:61  PROJECT 101AT                                  *
     H*                Do not allow entry of Fund or Employer without *
     H*                authority.                                     *
     H*                                                               *
     H*  05/10/99 C:33 SEQ#: WC051/17998                              *
     H*                IF DIV ARE RESTRICTED DO NOT ALLOW ACCESS.     *
     H*                                                               *
     H*  03/27/99 C:33 PROJECT: WC577                                 *
     H*                AVOID RECURSIVE CALLS IN SEARCES.              *
     H* _____________________________________________________________ *
     H*                                                               *
     H*              LJQ  8-23-91   ADDED SYSTEM    USER PARM FILE.   *
     H*                             FILE NEEDS TO BE UPDATED WHEN     *
     H*                             WC USER PARM IS UPDATED           *
     H*                                                               *
     H*  09/27/93 C:20   REQ/PROJ: SY007                              *
     H*                  1. ADDED F6=CO# SEARCH                       *
     H*                  2. ADDED F8=GROUP ALPHA SEARCH               *
     H*                  3. ADDED F18=GROUP # SEARCH                  *
     H*                                                               *
     H*  03/04/97 C:62   PROJ: WC586                                  *
     H*                  AFTER USING COMPANY SEARCH IF FUND SEARCHES  *
     H*                  ARE USED, CORRECT FUNDS ARE LISTED.          *
     H*                                                               *
     H*****************************************************************
      *                                                               *
      *                                                               *
     FW0006F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMUPL     UF A E           K DISK
     FSMUPL     UF A E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FSMCOL     IF   E           K DISK
     FWMEMP     IF   E             DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)

     D UN              S             40    DIM(1) CTDATA PERRCD(1)
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      ******* beg add ******* 06/10/14 *******************************
     d @@Rrn1          s              9
     d intMxRi         s              2  0
     d intCount        s              3  0
     d outMessage      s            256
     d outTitle        s             50
     d outBtnText      s             10
     d CnstMxRi        c                   const(8)

     d UpdateRecent    pr                  extpgm('STUPDRCNT')
     d   inRRN                        9
     d   inType                       1    const
     d   inQte                        1    const

     d/copy sprGenPop
      ******* end add ******* 06/10/14 *******************************
     d*** Beg Add *** 08/21/23 ***************************************
     d/copy sprModule
     d*** End Add *** 08/21/23 ***************************************

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
     C     UPKEY         KLIST
     C                   KFLD                    XXUSR            10
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C*
     C*   KEY LIST FOR THE FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
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
      ***** BEG DEL ******* 06/02/99 **********************************
     C*******************  MOVE Q1USER    SCUSER
      ***** END DEL ******* 06/02/99 **********************************
     C                   MOVE      UN(1)         SCPRDS
     C                   MOVE      UN(1)         QQPRDS           40
      ***** BEG ADD ******* 06/02/99 **********************************
      *
      * Center Screen Prompt in display field
      *
     C                   MOVEL(P)  SCPRDS        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRDS
      *
      ***** END ADD ******* 06/02/99 **********************************
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      Q1USER        XXUSR
      ***** BEG CHG ******* 03/08/22 **********************************
     C*****     UPKEY         CHAIN(N)  PUP01                              40         WMUPP
     C*****     UPKEY         CHAIN(N)  PUP02                              45         SMUPP
     C     UPKEY         CHAIN(N)  PUP02                              45         SMUPP
     C     UPKEY         CHAIN(N)  PUP01                              40         WMUPP
      ***** END CHG ******* 03/08/22 **********************************
     C     *IN40         IFEQ      '0'
     C*
     C***  BEGIN CHG  ***  05/10/99  *********************************
     C**********           MOVEA'000'     *IN,51
     C                   MOVEA     '0000'        *IN(51)
     C***  END   CHG  ***  05/10/99  *********************************
     C*
     C     UPXCO#        IFEQ      'Y'
     C     UPXFND        OREQ      'Y'
     C     UPXEMP        OREQ      'Y'
     C                   SETON                                            51
     C                   ENDIF
     C*
     C     UPXFND        IFEQ      'Y'
     C     UPXEMP        OREQ      'Y'
     C                   SETON                                            52
     C                   ENDIF
     C*
     C     UPXEMP        IFEQ      'Y'
     C                   SETON                                            53
     C***  BEGIN DEL  ***  05/16/00  *********************************
     C**********           ENDIF
     C***  END   DEL  ***  05/16/00  *********************************
     C*
     C***  BEGIN ADD  ***  05/10/99  *********************************
     C     UPFL5         IFEQ      'Y'
     C                   SETON                                            54
     C                   ENDIF
     C***  END   ADD  ***  05/10/99  *********************************
     C*
     C***  BEGIN ADD  ***  05/16/00  *********************************
     C                   ENDIF
     C***  END   ADD  ***  05/16/00  *********************************
     C*
     C                   MOVE      UPCO#         SCCO#
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPOUTQ        SCOUTQ
     C***  begin add  ***  03/01/16  **************************************
     C                   Move      UPPKey        scPrtKey
     C***  end add ******  03/01/16  **************************************
     C                   MOVE      UPEMP#        SCEMP#
     C                   MOVE      UPDIV         SCDIV
     C                   Z-ADD     UPFYR         SCYR
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT
     C***** BEG ADD ******* 04/19/06 **********************************
     C                   MOVEL(p)  UPFL4         SSSTAF
     C***** END ADD ******* 04/19/06 **********************************
      /free
       //****** beg add ********* 06/10/14 ****************************
       scMxRi = UPmxri;
       //****** end add ********* 06/10/14 ****************************
       //*** Beg Add ***** 11/06/23 *******************************
       scDsTab = UPdstab;
       scQST = UPqdflt;
       If UPdstab = 'P';
          scDsTab = 'Policy Search';
       Endif;
       If UPdstab = 'C';
          scDsTab = 'Claim Search';
       Endif;
       If UPdstab = 'Q';
          scDsTab = 'Quick Search';
       Endif;
       //*** End Add ***** 11/06/23 *******************************
      /end-free
     C*
     C                   ELSE
     C*
     C                   MOVE      *ZERO         SCCO#
     C                   MOVE      *ZERO         SCFND
     C                   MOVE      *BLANK        SCOUTQ
     C                   MOVE      *ZERO         SCEMP#
     C                   MOVE      *ZERO         SCYR
     C                   MOVE      *ZERO         SCDT
     C***** BEG ADD ******* 04/19/06 **********************************
     C                   MOVEL(p)  ' '           SSSTAF
     C***** END ADD ******* 04/19/06 **********************************
      /free
       //****** beg add ********* 06/10/14 ****************************
       clear scMxRi;
       //****** end add ********* 06/10/14 ****************************
      /end-free
     C*
     C                   END
      /free
       //****** beg add ********* 06/10/14 ****************************
       intMxRi = scMxRi;
       //****** end add ********* 06/10/14 ****************************
      /end-free
      ***** BEG ADD ******* 06/02/99 **********************************
      *
      * IF Fund/Employer Restriction records SMAZP exist,
      *   allow entry of default Fund/Employer
      *
     C                   CALL      'SA008R'
     C                   PARM                    SCCO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'W'           ##APPL            1
     C                   PARM      *OFF          ##RCDS            1
     C                   PARM      *BLANK        ##AUTH            1
      *
     C     *IN52         IFEQ      *ON
     C     *IN53         OREQ      *ON
      *
     C     ##RCDS        IFEQ      *ON
     C                   SETOFF                                       5253
     C                   ENDIF                                                  ##RTCD
      *
     C                   ENDIF                                                  *IN52, *IN53
      *
      ***** END ADD ******* 06/02/99 **********************************
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
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*** BEGIN ADD *** 09/28/93 **************************************
     C*
     C* F10=SEARCHES
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C***  BEGIN ADD  ***  03/27/99  *********************************
     C                   CLEAR                   XXPGM
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCCO#'
     C                   MOVEL(P)  'S0510R'      XXPGM
     C     HFLD          WHENEQ    'SCFND'
     C                   MOVEL(P)  'W0010R'      XXPGM
     C     HFLD          WHENEQ    'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C                   MOVEL(P)  'W0050R'      XXPGM
     C                   ENDSL
     C*
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM                    XXPGM            10
     C                   PARM                    XXERR             1
     C*
     C     XXERR         IFEQ      *ON
     C     XXPGM         ANDNE     *BLANKS
     C                   GOTO      STRT
     C                   ENDIF                                                  XXERR=*ON
     C***  END   ADD  ***  03/27/99  *********************************
     C*
     C* CO# SEARCH
     C*
     C     HFLD          IFEQ      'SCCO#'
     C     *IN51         ANDEQ     *OFF
     C*
     C                   CALL      'S0501R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL            1
      ***** BEG CHG ******* 05/13/02 **********************************
     C*********            PARM           SCCO#
     C                   PARM      *ZERO         ##CO#             3 0
      ***** END CHG ******* 05/13/02 **********************************
     C                   PARM                    QQRTC             1
      *
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
      ***** BEG CHG ******* 05/13/02 **********************************
     C*********  SCCO#     OREQ *ZERO
     C     ##CO#         OREQ      *ZERO
      ***** END CHG ******* 05/13/02 **********************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN29
     C*** BEGIN ADD *** 03/04/97 **************************************
     C     UPKEY         CHAIN     PUP01                              60
      ***** BEG ADD ******* 05/13/02 **********************************
     C                   Z-ADD     ##CO#         SCCO#
      ***** END ADD ******* 05/13/02 **********************************
     C                   MOVE      SCCO#         UPCO#
     C                   UPDATE    PUP01
     C*** END   ADD *** 03/04/97 **************************************
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END *INKF
      *
      *  Group Search
      *
     C     HFLD          IFEQ      'SCFND'
     C     *IN52         ANDEQ     *OFF
      ***** BEG ADD ******* 05/13/02 **********************************
      *
     C     SCCO#         IFEQ      *ZERO
     C                   SETON                                        73
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *
      ***** END ADD ******* 05/13/02 **********************************
     C                   MOVEL     SCCO#         @@CO#
     C                   CALL      'W0010R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@CO#             3
      ***** BEG CHG ******* 05/13/02 **********************************
     C*********            PARM           @@FND   3
     C                   PARM      *BLANKS       @@FND             3
      ***** END CHG ******* 05/13/02 **********************************
     C                   PARM                    @@FYR             3
     C                   PARM      1             @@VIEW            1 0
     C*
      ***** BEG DEL ******* 05/13/02 **********************************
     C*********            MOVE @@FND     SCFND
      ***** END DEL ******* 05/13/02 **********************************
      ***** BEG ADD ******* 05/13/02 **********************************
     C                   MOVE      @@FND         ##FND             3 0
      ***** END ADD ******* 05/13/02 **********************************
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      'B'
      ***** BEG CHG ******* 05/13/02 **********************************
     C*********  SCFND     OREQ *ZERO
     C     ##FND         OREQ      *ZERO
      ***** END CHG ******* 05/13/02 **********************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   ELSE
     C*
      ***** BEG ADD ******* 05/13/02 **********************************
     C                   Z-ADD     ##FND         SCFND
      ***** END ADD ******* 05/13/02 **********************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C     HFLD          IFEQ      'SCEMP#'
     C     *IN53         ANDEQ     *OFF
     C     HFLD          OREQ      'SCDIV'
     C     *IN53         ANDEQ     *OFF
     C*
     C     SCCO#         IFEQ      *ZEROS
     C     SCFND         OREQ      *ZEROS
     C                   MOVE      *ON           *IN31
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   MOVE      SCCO#         XXCO#
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
      ***** BEG CHG ******* 05/13/02 **********************************
     C*********            PARM           @@RRN   90
     C                   PARM      *ZERO         @@RRN             9 0
      ***** END CHG ******* 05/13/02 **********************************
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      2             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZERO
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C     @@RRN         CHAIN     WMEMP                              60
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      ***** BEG ADD ******* 05/13/02 **********************************
     C                   SETON                                        74
      ***** END ADD ******* 05/13/02 **********************************
     C                   GOTO      STRT
     C                   END
     C*
     C                   ENDIF
     C*
     C                   ENDIF
      ***** BEG ADD ******* 05/13/02 **********************************
      *
      *  Group Policy Year Search
      *
     C     HFLD          IFEQ      'SCYR'
      *
     C     SCCO#         IFEQ      *ZEROS
     C     SCFND         OREQ      *ZEROS
     C                   MOVE      *ON           *IN76
     C                   GOTO      STRT
      *
     C                   ELSE
      *
     C                   MOVE      SCCO#         XXCO#
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
      *
     C                   CALL      'W0011R'
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      DBRRN         ##RRN             9 0
     C                   PARM      *ZERO         ##YR              3 0
      *
     C     ##YR          IFEQ      *ZERO
     C                   SETON                                        74
     C                   ELSE
     C                   Z-ADD     ##YR          SCYR
     C                   SETON                                        75
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  ##YR
      *
     C                   ENDIF
      *
     C                   ENDIF                                                  HFLD = SCYR
      *
      *
     C     HFLD          IFEQ      'SCDT'
     C                   SETON                                        77
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = SCYR
      *
     C     HFLD          IFEQ      'SCOUTQ'
     C                   SETON                                        78
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = SCYR
      *
      *
      ***** END ADD ******* 05/13/02 **********************************
     C*
     C                   GOTO      STRT
     C                   END
     C*** END ADD *** 09/28/93 ****************************************
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C     SCR01T        TAG
     C*
     C*  THE COMPANY NUMBER MUST BE VALID
     C*
     C                   Z-ADD     SCCO#         XXCO#
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE STATUS CODE MUST BE 'A'
     C*
     C     COSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE FUND NUMBER MUST BE VALID
     C*
     C                   Z-ADD     SCCO#         XXCO#
     C                   Z-ADD     SCFND         XXFND
     C     SCFND         IFNE      0
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
      ***** BEG ADD ******* 06/02/99 **********************************
      *
      * Check for Fund Restriction
      *
     C     ##RCDS        IFEQ      *ON
     C     UPXFND        ANDEQ     'Y'
     C                   CALL      'SA008R'
     C                   PARM                    SCCO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'W'           ##APPL            1
     C     *IN71         PARM      *OFF          ##RTCD            1
     C                   PARM      *BLANK        ##AUTH            1
     C                   PARM                    SCFND
      *
     C   71              GOTO      STRT
     C                   ENDIF                                                  UPXFND
      *
      * Check for Employer Restriction
      *
     C     ##RCDS        IFEQ      *ON
     C     UPXEMP        ANDEQ     'Y'
     C                   CALL      'SA008R'
     C                   PARM                    SCCO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'W'           ##APPL            1
     C     *IN72         PARM      *OFF          ##RTCD            1
     C                   PARM      *BLANK        ##AUTH            1
     C                   PARM                    SCFND
     C                   PARM                    SCEMP#
      *
     C   72              GOTO      STRT
     C                   ENDIF                                                  UPXEMP
      *
      ***** END ADD ******* 06/02/99 **********************************
     C*
     C*  THE STATUS CODE MUST BE 'A'
     C*
     C     FNSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   CHECK THE PROCESS DATE FOR A VALID DATE
     C*
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK THE OUTQ FOR A VALID VALUE
     C*
     C                   CALL      'S0003C'
     C                   PARM                    SCOUTQ
     C                   PARM      ' '           XXFOND            1
     C     XXFOND        IFEQ      '1'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C***** BEG ADD ******* 04/19/06 **********************************
     C*  Claim subset/staff code must be Y, N or blank
     C*
     C     SSSTAF        IFNE      'Y'
     C     SSSTAF        ANDNE     'N'
     C     SSSTAF        ANDNE     ' '
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C***** END ADD ******* 04/19/06 **********************************
     C***** Beg Add ******* 11/06/23 **********************************
     C     SCDSTAB       IFEQ      'Quick Search'
     C     SCQST         ANDEQ     ' '
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C***** End Add ******* 11/06/23 **********************************
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C***  Begin Add  ***  11/22/17  **************************************
     C                   Clear                   UPsub#
     C***  End   Add  ***  11/22/17  **************************************
     C     UPKEY         CHAIN     PUP01                              40         WMUPP
     C     UPKEY         CHAIN     PUP02                              45         SMUPP
     C*
     C                   MOVE      Q1USER        UPUSR
     C                   MOVE      SCCO#         UPCO#
     C                   MOVE      SCFND         UPFND
     C                   Z-ADD     SCYR          UPFYR
     C                   MOVE      SCOUTQ        UPOUTQ
     C***  begin add  ***  03/01/16  **************************************
     C                   Move      scPrtKey      UPPKey
     C***  end add ******  03/01/16  **************************************
     C                   MOVE      SCEMP#        UPEMP#
     C                   MOVE      SCDIV         UPDIV
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPPRDT
      **** BEG CHG **** 05/30/03 *****************************************
      * On ADD or UPDATE, load Status with and "A"
     C***********          MOVE *BLANK    UPSTAT
     C                   MOVE      'A'           UPSTAT                         Active Status
      **** END CHG **** 05/30/03 *****************************************
     C***** BEG ADD ******* 04/19/06 **********************************
     C                   MOVE(p)   SSSTAF        UPFL4
     C***** END ADD ******* 04/19/06 **********************************
      /free
       //****** beg add ********* 06/10/14 ****************************
       UPmxri = scMxRi;
       //*** Beg Add ***** 11/06/23 *******************************
       If scDsTab = *blanks;
          UPdstab = ' ';
          UPqdflt = ' ';
       Endif;
       If scDsTab = 'Policy Search';
          UPdstab = 'P';
          UPqdflt = ' ';
       Endif;
       If scDsTab = 'Claim Search';
          UPdstab = 'C';
          UPqdflt = ' ';
       Endif;
       If scDsTab = 'Quick Search';
          UPdstab = 'Q';
          UPqdflt = scQST;
       Endif;
       //*** End Add ***** 11/06/23 *******************************
       clear outMessage;
       outTitle   = 'Information';
       outBtnText = 'Ok';

       If UPmxri > 0 and UPmxri > intMxRi;
          outMessage = 'When the ''' + 'Maximum # of Recent Items''' +
           ' is increased, the system will begin adding new records to the +
           grid from that point forward. It will not automatically retrieve +
           older items.';
          PopOneBtn(outMessage:outTitle:outBtnText);
       Endif;
       //****** end add ********* 06/10/14 ****************************
      /end-free
     C                   MOVE      Q1USER        UPUSER
     C                   MOVE      Q1PGM         UPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPDT
     C                   Z-ADD     DSTIME        UPTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PUP01
     C                   ELSE
     C                   WRITE     PUP01
     C                   END
     C*
     C***  Begin Add  ***  11/22/17  **************************************
       //*** Beg Chg *** 08/21/23 *****************************************
     C***********        If        UPsub# = *zero
       If Not isModEnabled('Managed Care');
       //*** End Chg *** 08/21/23 *****************************************
     C                   Clear                   UPfnd
     C                   Endif
     C***  End   Add  ***  11/22/17  **************************************
     C     *IN45         IFEQ      '0'                                          LJQ 8-23-91
     C                   UPDATE    PUP02                                        |
     C                   ELSE                                                   |
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPIDT
     C                   MOVE      Q1USER        UPIUSR                         |
     C                   WRITE     PUP02                                        |
     C                   END                                                    |
     C*
      /free
       //****** beg add ********* 06/10/14 ****************************
       If (UPmxri < intMxRi) or (intMxRi = 0 and UPmxRi < CnstMxRi);
          @@Rrn1 = '000000000';
          UpdateRecent(@@Rrn1:'P':' ');
       Endif;
       //****** end add ********* 06/10/14 ****************************
      /end-free

     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**  TABLE UN
        Change User Parameters
