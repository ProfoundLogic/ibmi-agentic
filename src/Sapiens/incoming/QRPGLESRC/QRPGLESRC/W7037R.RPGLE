     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     h/DEFINE   PROFOUNDUI
     H*
     H/TITLE W7037R - CLT121 - CLARENDON BANK DOWNLOAD
     H*****************************************************************
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*      MENU                                                     *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*      W7037C - SUBMIT                                          *
     H*                                                               *
     H* Created  10/28/03  C:132 PROJECT: 121WAR                      *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/31/04  C: 53  PROJECT: 121WBB
     H*                  CREATE IN CLT129
     H*                                                               *
     H* 12/01/03  C:132  PROJECT: WA121  SEQ: 40139                   *
     H*                  Change Call Program from W2420R to W2422R    *
     H*                  to allow viewing all bank accounts.          *
     H*                                                               *
     H*****************************************************************
      *
      * Prompt screen
     FW7037F    CF   E             WORKSTN Handler('PROFOUNDUI(HANDLER)')
     F                                     INFDS(FIDS01)
      *
      * System Company
     FSMCOL     IF   E           K DISK
      *
      * User Parameter
     FWMUPL     IF   E           K DISK
      *
      *  WCMS Bank Account Master
     FWSCKL1    IF   E           K DISK
      *
      *
      *

     D OPTNM           C                   CONST('Clarendon Bank Down-
     D                                     load')
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *
      *==================================================================
      * KLISTS
      *==================================================================
      *
      *  Company Master
      *

     C     COKY          KLIST
     C                   KFLD                    UPCO#
      *
      *  User Parameter
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *  Bank Account Master
      *
     C     CKKEY         KLIST
     C                   KFLD                    SCACCT
      *
      *
      *
      *=================================================================
      * *LIKE DEFN
      *=================================================================
     C     *LIKE         DEFINE    UPDT          XXFRDT
     C     *LIKE         DEFINE    UPDT          XXTODT
      *
      *
      *
      *=================================================================
      *  PLISTS
      *=================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@PRDS           40
     C                   PARM                    @@CALL            1
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
      *
      *
      *===============================================================
     C     STRT01        TAG
      *
      *  Get User Company
      *
     C     UPKY          CHAIN     PUP01                              60
     C   60              MOVE      *ZERO         UPCO#
      *
      *  Get the Company Name
      *
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL(P)  SRDFLD        SCCONM
      *
     C                   TIME                    DSDTTM
      *
      *  Load Pgm Title
      *
     C     Q1PRMS        IFGE      1
     C                   MOVEL(P)  @@PRDS        SCPRDS
     C                   ELSE
     C                   MOVEL(P)  OPTNM         SCPRDS
     C                   ENDIF
      *
     C                   MOVEL(P)  SCPRDS        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRDS
      *
      *   Load the Record Format Info For First Time
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
      *   Display All Formats
      *
     C     STRT          TAG
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF                                                  Q2RECF=SCR01
      *
      *   Process All Command KEYS
      *
     C     *IN11         IFEQ      *ON
      *
      *   F3=EXIT
      *
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC=*ON
      *
      *   F10=Search
      *
     C   KJ              EXSR      KJDO
      *
     C                   ENDIF                                                  *IN11=*ON
      *
      *
      *
      *********************
      *  Error Check SCR01
      *********************
      *
      *   Both dates must be entered
      *
     C                   SELECT
      *
     C     SCFRDT        WHENEQ    *ZERO
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
      *
     C     SCTODT        WHENEQ    *ZERO
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
      *
     C                   ENDSL
      *
      *   Validate the Dates Entered
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
      *
     C     SRERR         CABEQ     '1'           STRT                     20
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
      *
     C     SRERR         CABEQ     '1'           STRT                     24
      *
      *  Dates must be in ascending order.
      *
     C     XXTODT        IFLT      XXFRDT
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF                                                  SCTODT IFLT SCFRDT
      *
      *  The Bank Account Number must exist in Account Master
      *
     C     CKKEY         CHAIN     PCK01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   SETON                                        74
     C                   GOTO      STRT
     C                   ENDIF
      *
      * Call the CLP to Submit the List
      *
     C                   MOVE      SCFRDT        ##FRDT            8
     C                   MOVE      SCTODT        ##TODT            8
     C                   MOVE      SCACCT        ##ACCT           20
      *
     C                   CALL      'W7037C'
     C                   PARM                    ##FRDT            8
     C                   PARM                    ##TODT            8
     C                   PARM                    ##ACCT           20
      *
      *  END THE PROGRAM
      *
      *===================== *
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
      *
      *===============================================================
      * KJDO - SEARCH
      *===============================================================
     C     KJDO          BEGSR
     C                   MOVE(P)   Q2RECF        XXRECF           10
      *
     C                   SELECT
      *
      * FROM Date
      *
     C     HFLD          WHENEQ    'SCFRDT'
     C                   SETON                                        72
      *
      * TO Date
      *
     C     HFLD          WHENEQ    'SCTODT'
     C                   SETON                                        73
      *
      * ACCOUNT NUBMER
      *
     C     HFLD          WHENEQ    'SCACCT'
      *
      ***** BEG CHG ***** 12/01/03 ***************************************
     C*************        CALL 'W2420R'
     C*************        PARM '2'       #1CALL  1
     C*************        PARM *BLANK    #1KC    1
     C*************        PARM *BLANK    #1KL    1
     C*************        PARM *BLANKS   #1CO#   3
     C*************        PARM *BLANKS   #1FND   3
     C*************        PARM *BLANKS   #1ACCT 20
     C*************        PARM *BLANKS   #1EFDT  8
     C*************        PARM *BLANKS   #1APPL  2
     C*************        PARM *BLANKS   #1CKTP 10
      *
     C                   CALL      'W2422R'
     C                   PARM      *BLANK        #1KC              1
     C                   PARM      *BLANK        #1KL              1
     C                   PARM      '2'           #1CALL            1
     C                   PARM      *BLANKS       #1CO#             3
     C                   PARM      *BLANKS       #1ACCT           20
      ***** END CHG ***** 12/01/03 ***************************************
      *
     C     #1ACCT        IFNE      *BLANKS
     C                   MOVE      #1ACCT        SCACCT
     C                   SETON                                        71
      *
     C                   ELSE
      *
     C                   SETON                                        71
     C                   ENDIF
      *
     C                   WRITE     SCR99
      *
     C                   ENDSL
      *
     C                   MOVE(P)   XXRECF        Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDSR                                                  KJDO
      *
      * =========================================================== *
