     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE WA062R - API TO WRITE AUTO-DARTS REMARKS
     H*****************************************************************
     H*  Description:
     H*
     H*  This API will write DARTS remarks on CLAIM updation
     H* *****************************************************************
     H*  Called From:                                                   *
     H*    VARIOUS PROGRAMS                                             *
     H*  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     H*  Calls To:                                                      *
     H*    DA700R - Write DARTS API                                     *
     H*                                                                 *
     H* *****************************************************************
     H*  Created:   7/14/09  C:189 Project:  C114WOJ
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*  07/16/18 C:062  Project 133WQO                               *
     H*                  Don't display screen upon add.               *
     H*                                                               *
     H*  07/18/14 C:282  Redmine: 4554                                *
     H*                  Default Darts Remark Code exist in Remark    *
     H*                  type table record condtion is removed.       *
     H*                                                               *
     H*  04/08/14 C:290  Support: WC147:85254                         *
     H*                  Changed to print the correct group info on   *
     H*                  the 'reason for the change' screen.          *
     H*                                                               *
     H*  10/04/11 C:279  Project: 133WLX                              *
     H*                  Changed to update all claims for same SS# if *
     H*                  name is changed with DARTS remarks.          *
     H*
     H*  10/03/11 C:278  Project: 133WLW                              *
     H*                  The Dart Remark will be updated to           *
     H*                  include the claimant's old address, along    *
     H*                  with the reason for the change.              *
     H*                                                               *
     H*****************************************************************
     H*  Indicators:
     H*
     H*
     H* ****************************************************************
     F*
     F* ****************************************************************
     F*      F I L E S
     F* ****************************************************************
     F*
     H*   Claim Master File  - WMCML1                              *
     FWMCML1    IF   E           K DISK
     FWMPDL     IF   E           K DISK
     F*** BEG  ADD ***  10/04/11  ************************************
     FWMCMLB    IF   E           K DISK
     F                                     RENAME(PCM01:PCM0B)
     F*** END  ADD ***  10/04/11  ************************************
     F*
     F*   Remark table
     FSTCTL1    IF   E           K DISK
     F*
     F*   Users by staf code.
     FWMUPL     IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWA074F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*

     D* -----------------------------------------------------------
     D*    A r r a y s
     D* -----------------------------------------------------------
     D*============================================================
     D*    D A T A
     D*============================================================
     D*    DARTS key for claim.
     D                 DS
     D  DSKEY                  1     40
     D  CMCO#                  1      3  0
     D  CMFND                  4      6  0
     D  CMFYR                  7      9  0
     D  CMCASE                10     16  0
     D*
     D                 DS
     D  DSKEY1                 1     40
     D  PDFEIN                 1      9  0
     D  PDMOD                 10     12  0
     D*    System date and time.
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** BEG  ADD ***  10/03/11  ************************************
     D CLADDR          C                   CONST('OLD CLAIMANT ADDRES-
     D                                     S :')

     I*** END  ADD ***  10/03/11  ************************************

     C*
     C* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C*       PARAMETER LISTS
     C* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C     *ENTRY        PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##FYR             3 0
     C                   PARM                    ##CASE            7 0
     C                   PARM                    ##FEIN            9 0
     C                   PARM                    ##MOD             3 0
     C                   PARM                    ##RCOD            5
     C                   PARM                    ##RTN             1
     C*** BEG  ADD ***  10/03/11  ************************************
     C                   PARM                    ##XHAD          116
     C*** END  ADD ***  10/03/11  ************************************
     C*** BEG  ADD ***  10/04/11  ************************************
     C                   PARM                    ##CHG             1
     C*** END  ADD ***  10/04/11  ************************************
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
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
     C* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C*       K  E  Y      L  I  S  T  S
     C* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C*   KEY LIST FOR THE REMARK TYPE TABLE FILE
     C     RMKEY         KLIST
     C                   KFLD                    ##RCOD            5
     C*
     C*   KEY LIST FOR CLAIM MASTER - WMCML1
     C     CLMKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*** BEG  ADD ***  10/04/11  ************************************
      *  Key list for the Claim Master file
      *
     C     CMKYB         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSS#
     C*** END  ADD ***  10/04/11  ************************************
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     PDKEY         KLIST
     C                   KFLD                    ##FEIN
     C                   KFLD                    ##MOD
     C*
     C* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C*       DATA DEFINITIONS
     C* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C     *LIKE         DEFINE    CMDT          XXDT
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C*** BEG  ADD ***  10/04/11  ************************************
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C*** END  ADD ***  10/04/11  ************************************
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    UPSTAF        KKSTAF
     C*============================================================
     C*    M A I N      L O G I C
     C*============================================================
     C*
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP02                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
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
     C***** Beg Add *** 04/08/14 *************************************
      /free
       If ##FND <> *zero;
          scFND = ##FND;
       Else;
      /end-free
     C***** End Add *** 04/08/14 *************************************
     C                   MOVE      UPFND         SCFND
     C***** Beg Add *** 04/08/14 *************************************
      /free
       Endif;
      /end-free
     C***** End Add *** 04/08/14 *************************************
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*** Begin Add *** 07/16/18 ********************************
     C*
     C* Don't display screen
     C*
     C     Q1PRMS        IFGE      10
     C     ##CHG         ANDEQ     'A'
     C                   GoTo      scr01t
     C                   EndIf
     C*** End   Add *** 07/16/18 ********************************
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   RETURN
     C                   END
     C*
     C     SCRESN        IFEQ      *BLANKS
     C                   SETON                                        21
     C                   GOTO      STRT
     C                   END
     C*
     C*** BEG  ADD ***  10/04/11  ************************************
     C                   MOVE      *ON           @@FRST            1
     C*** END  ADD ***  10/04/11  ************************************
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*    Set up keys, access control and table files
     C                   Z-ADD     ##CO#         KKCO#
     C                   Z-ADD     ##FND         KKFND
     C                   Z-ADD     ##FYR         KKFYR
     C                   Z-ADD     ##CASE        KKCASE
     C*
     C*    Get claim master.
     C*
     C     ##CASE        IFNE      0
     C     CLMKY         CHAIN     PCM01                              60
     C                   ELSE
     C     PDKEY         CHAIN     PPD01                              60
     C                   ENDIF
     C*
     C     *IN60         CABEQ     *ON           ENDAP
     C*    Get Remark Table record.
     C*
     C     RMKEY         CHAIN     PCT01                              60
     C*
     C*** BEG  DEL ***  07/18/14  ************************************
     C*****     *IN60         CABEQ     *ON           ENDRD
     C*** END  DEL ***  07/18/14  ************************************
     C     CTSTAT        CABEQ     'I'           ENDRD
     C*
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*      SELECT AND PROCESS
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*
     C*** BEG  ADD ***  10/04/11  ************************************
     C*  If the claimant name was changed, update all claims with the
     C*    same SS#.
     C     *ZEROS        IFNE      ##FEIN
     C                   MOVE      *OFF          @@FRST
     C*** END  ADD ***  10/04/11  ************************************
     C*    Execute DARTS API
     C                   EXSR      DRAPI
     C*** BEG  ADD ***  10/04/11  ************************************
     C                   ELSE
     C                   MOVEL     CMSS#         KKSS#
     C     CMKYB         SETLL     PCM0B
     C     CMKYB         READE     PCM0B                                  60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      DRAPI
     C     CMKYB         READE     PCM0B                                  60
     C                   ENDDO
     C                   ENDIF
     C*
     C     Q1PRMS        IFGE      10
     C     ##CHG         ANDEQ     'Y'
     C     @@FRST        ANDEQ     *ON
     C                   MOVE      *OFF          @@FRST
     C                   MOVE      *BLANKS       ##XHAD
     C                   GOTO      SCR01T
     C                   ENDIF                                                  Q1PRMS >=10
     C*** END  ADD ***  10/04/11  ************************************
     C*
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*
     C*    Next record for this key
     C     ENDRD         TAG
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*
     C*    Return to caller.
     C     ENDAP         TAG
     C                   MOVE      *BLANKS       SCRESN
     C                   SETON                                        LR
     C                   RETURN
     C*
     C*================================================================
     C*    DRAPI - CALL DARTS API DA700R
     C*================================================================
     C     DRAPI         BEGSR
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C*
     C     ##CASE        IFNE      0
     C                   MOVEL(P)  DSKEY         @@KEY            40              1
     C                   ELSE
     C                   MOVEL(P)  DSKEY1        @@KEY            40              1
     C                   ENDIF
     C*
     C                   MOVEL(P)  SCRESN        @@COMM          256              2
     C     @@COMM        IFEQ      *BLANKS
     C                   MOVEL(P)  CTDESC        @@COMM
     C                   ENDIF                                                  e COMM = ' '
     C*** BEG  ADD ***  10/03/11  ************************************
     C     Q1PRMS        IFGE      9
     C     ##XHAD        IFNE      *BLANKS
     C                   CAT(P)    CLADDR:1      @@COMM
     C                   CAT(P)    ##XHAD:1      @@COMM
     C                   ENDIF                                                  ##XHAD <> ' '
     C                   ENDIF                                                  Q1PRMS >= 9
     C*** END  ADD ***  10/03/11  ************************************
     C*
     C*          ------------------------------------------------------
     C*     Diary data for diary type remarks
     C     CTFL01        IFEQ      'Y'
     C                   EXSR      GETDAT
     C     @@CDAT        CABEQ     *ZERO         ENDAPI
     C                   MOVEL(P)  Q1USER        @@USER           10              7
     C*
     C                   ELSE
     C                   MOVE      DSDATE        @@CDAT            8
     C                   MOVEL(P)  Q1USER        @@USER
     C*
     C                   ENDIF                                                  e FL01=Y
     C*
     C                   MOVEL(P)  CTDEPG        @@DCID           12              4
     C                   MOVEL(P)  'W0070R'      @@PNAM           10              5
     C                   MOVEL(P)  Q1PGM         @@PGM            10              8
     C     CTDEPG        IFNE      *BLANKS
     C                   MOVEL(P)  DSKEY         @@FOLD           80              9
     C                   ELSE                                                   x DEPG NE ' '
     C                   MOVEL(P)  *BLANKS       @@FOLD
     C                   ENDIF                                                  e DEPG NE ' '
     C                   MOVEL(P)  CTFL01        @@DIFL            1             10
     C                   MOVEL(P)  Q1PGM         @@IPGM           10             11
     C     CTDEPG        IFNE      *BLANKS
     C                   MOVE(P)   'Y'           @@FL01            1             12
     C                   ELSE                                                   DCID <> blanks
     C                   MOVE      'N'           @@FL01                          12
     C                   ENDIF                                                  DCID <> blanks
     C     CTDPRG        IFGT      0
     C                   MOVE(P)   'Y'           @@FL02            1             13
     C                   ELSE                                                   DPRG > 0
     C                   MOVE      'N'           @@FL02                          13
     C                   ENDIF                                                  DPRG > 0
     C                   MOVEL(P)  ##RCOD        @@RMCD            5             14
     C                   MOVE      '00000'       @@SEQ#            5             15
     C                   MOVEL(P)  'Y'           @@SBYL            1             16
     C                   MOVEL     'Y'           ##RTN
     C*          ------------------------------------------------------
     C                   CALL      'DA700R'
     C                   PARM                    @@KEY
     C                   PARM                    @@COMM          256
     C                   PARM                    @@CDAT
     C                   PARM                    @@DCID
     C                   PARM                    @@PNAM
     C                   PARM                    @@SECL            3
     C                   PARM      Q1USER        @@USER
     C                   PARM      Q1PGM         @@PGM
     C                   PARM                    @@FOLD
     C                   PARM                    @@DIFL
     C                   PARM                    @@IPGM
     C                   PARM                    @@FL01
     C                   PARM                    @@FL02
     C                   PARM                    ##RCOD
     C                   PARM                    @@SEQ#
     C                   PARM                    @@SBYL
     C*          ------------------------------------------------------
     C     ENDAPI        ENDSR
     C*
     C*================================================================
     C*    GETDAT - Calculate CDAT for remark/diary
     C*================================================================
     C     GETDAT        BEGSR
     C                   MOVE      DSDATE        @@CDAT
     C*
     C                   ENDSR
     C*================================================================
     C*    *INZSR - INITIALIZATION SUBROUTINE
     C*================================================================
     C     *INZSR        BEGSR
     C*
     C* GET TODAY'S DATE
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C                   ENDSR
     C*================================================================
