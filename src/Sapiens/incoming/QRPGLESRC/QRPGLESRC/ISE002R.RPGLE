     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*      USER EXIT - AUTO-DIARY/ROUTE
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/30/21 c:62 Ticket 95217(152)                               *
     H*               Prevent locks.                                  *
     H*                                                               *
     H*  9/20/13 - IMPORT FIX. - ASSURE "RL" IS USED FOR IEDSTS
      *
     F* 07/09/07 C:191 PROJECT: 10IJI                                 *
     F*               -WRITE RECEIVED DATE TO IMIEP                   *
     H***                                                             *
     H*****************************************************************
     H* FILE SPECIFICATIONS  * * *
     H*****************************************************************
     H* INDICATOR USAGE
     H* SCR01:
     H* N98      EITHER NO SECURITY OR MEETS SECURITY LEVEL
     H*  98      PROTECT REMARK TYPE BECAUSE OF SECURITY LEVEL
     H*  99      GLOBAL - IMAGE PRODUCT IS INSTALLED
     H*****************************************************************
     F*
     FSDCMLA    UF   E           K DISK
     F                                     RENAME(PCM01:PCM04)
     FWMCML1    IF   E           K DISK
     FIME2L2    IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F*
     FIMIELH    UF A E           K DISK
     F*** BEG ADD ********** 07/09/07 ********************************
     F*** LOGICAL OF IMDOP
     FIMDOL1    IF   E           K DISK

     D*** END ADD ********** 07/09/07 ********************************
     D QS              S             10    DIM(99)                              Q'S SELCTD
     D DN              S             75    DIM(4)                               DOCN's
     D*
     D MDOC            C                   CONST('Document:')
     D MHAS            C                   CONST('has arrived on')
     D MQUE            C                   CONST('Queue:')
     D MSEN            C                   CONST('Sent by:')
     D MID             C                   CONST('&QUEUE  ID')
     D DDAT            C                   CONST('Date:')
     D DTIM            C                   CONST('Time:')
     D DREA            C                   CONST('REASSIGN')
     D DHBR            C                   CONST('has been reassigned')
     D DTOQ            C                   CONST('to Queue:')
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  SCMSG1                 1     64
     D  SM                     1     64
     D                                     DIM(64)                              SCMSG

     I*
     IPCM04
     I              CMKEY                       PIKEY
     I              CMSEQ#                      PISEQ#
     I              CMRMCD                      PIRMCD
     I*

     C     *LIKE         DEFINE    CMDT          XXDATE
     C*
     C     CMKYA         KLIST
     C                   KFLD                    QQKEY            40
     C                   KFLD                    ##SEQ#            5 0
     C*
     C     UPKY1         KLIST
     C                   KFLD                    XXFEAD
     C*
     C*
     C     UPKY2         KLIST
     C                   KFLD                    XXSUPR
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     E2KY          KLIST
     C                   KFLD                    XXFND
     C                   KFLD                    XXRMCD            5
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     CMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#            5 0
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     IEKY          KLIST
     C                   KFLD                    FQ
     C                   KFLD                    DCID
     C*
     C*
     C     IEKY1         KLIST
     C                   KFLD                    ##QID            10
     C                   KFLD                    QQDCID
     *** BEG ADD ********** 07/09/07 ********************************
     C     DOKY          KLIST
     C                   KFLD                    QQDCID
     *** END ADD ********** 07/09/07 ********************************
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8               MDY(YY)
     C                   PARM      *BLANKS       SRTFMT            8               MDYY
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8            MDYY
     C                   PARM      '*YYMD'       SRTFMT            8            YYYYMMDD
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
     C*
     C     DTYMD         PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8            MDYY
     C                   PARM      '*YMD'        SRTFMT            8            YYYYMMDD
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8            MDYY
     C                   PARM      '*CYMD'       SRTFMT            8            CYYMMDD
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
     C*
     C     *ENTRY        PLIST
     C                   PARM                    FQ               10             FROM QUEUE
     C                   PARM                    TU               10             TO USER
     C                   PARM                    USRID            10             FROM USRID
     C                   PARM                    DCID             12             DOC ID
     C                   PARM                    KEY              40             DART KEY
     C                   PARM                    SEQ               5                  SEQ
     C                   PARM                    RTRN              6              RTN CODES
     C*
     C                   MOVEL     *BLANKS       RTRN
     C                   MOVEL     DCID          QQDCID
     C                   MOVEL     FQ            QQINQ
     C                   MOVEL     KEY           QQKEY
     C                   MOVE      SEQ           ##SEQ#
     C                   MOVE      *BLANKS       QS                             CLEAR SLCTD QS
     C                   MOVE      *BLANKS       QTUSR            10                        QS
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C* * * *  IS THE INDEX A VALID CLAIM INDEX???
     C*** BREAK OUT KEY VALUE TO A VALID CLAIM KEY FOR CHECKING
     C*
     C     3             SUBST     KEY:1         ##CO#             3
     C     3             SUBST     KEY:4         ##FND             3
     C     3             SUBST     KEY:7         ##FYR             3
     C     7             SUBST     KEY:10        ##CASE            7
     C                   TESTN                   ##CO#                71
     C   71              TESTN                   ##FND                71
     C   71              TESTN                   ##FYR                71
     C   71              TESTN                   ##CASE               71
     C     *IN71         IFNE      *ON
     C                   MOVEL     '003   '      RTRN
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C                   MOVE      ##CO#         XXCO#             3 0
     C                   MOVE      ##FND         XXFND             3 0
     C                   MOVE      ##FYR         XXFYR             3 0
     C                   MOVE      ##CASE        XXCASE            7 0
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '001 CM'      RTRN
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*** GET THE CLAIM SUPERVISOR AND CLAIM ADJUSTER STAFF CODES,
     C****  THEN, GET THE USER ID'S FOR EACH TO USE FOR ROUTING AND FOR
     C****  DIARY...
     C*
      *
      *
     C********* DEL ****   MOVELCMFEAD    XXFEAD  3
     C                   MOVEL     CMCLEX        XXFEAD            3
     C                   MOVEL     CMSUPR        XXSUPR            3
      *
     C     UPKY1         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     '001 UP  '    RTRN
     C                   MOVEL     *BLANKS       @@ADJN           10
     C                   ELSE
     C                   MOVEL     UPUSR         @@ADJN           10
     C                   ENDIF
      *
      *
     C     UPKY2         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     '002 UP  '    RTRN
     C                   MOVEL     *BLANKS       @@SUPN           10
     C                   ELSE
     C                   MOVEL     UPUSR         @@SUPN           10
     C                   ENDIF
      *
      *
      *
     C*** CHAIN TO THE USER PARM FILE WITH STAFF CODE TO GET THE USRIDS
     C*
     C*** Begin Chg *** 03/30/21 *******************************************
     C*****CMKYA         CHAIN     PCM04                              60
     C     CMKYA         CHAIN(N)  PCM04                              60
     C*** End   Chg *** 03/30/21 *******************************************
     C     *IN60         IFEQ      *ON
     C                   MOVE      '001 FN'      RTRN
     C                   GOTO      TERM
     C                   ELSE
     C                   MOVEL     PIRMCD        XXRMCD
     C                   ENDIF
     C*
     C*** CHAIN OUT TO IME2P FILE TO GET THE ACTION AND FOR WHICH
     C*** DEPARTMENT (ADJ/SUPER)
     C*
     C     E2KY          CHAIN     PE201                              60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     *ZEROS        XXFND
     C     E2KY          CHAIN     PE201                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '001 FN'      RTRN
     C                   GOTO      TERM
     C                   ENDIF
     C                   ENDIF
     C*
     C* If the E2AACT field contains B or D, do a diary to adjustor
     C*
     C     TU            IFEQ      *BLANKS
     C     E2AACT        IFEQ      'B'
     C     E2AACT        OREQ      'D'
     C                   EXSR      DADJ
     C                   ENDIF
     C                   ENDIF
     C*
     C* If the E2SACT field contains B or D, do a diary to supervisor
     C*
     C     TU            IFEQ      *BLANKS
     C     E2SACT        IFEQ      'B'
     C     E2SACT        OREQ      'D'
     C                   EXSR      DSUP
     C                   ENDIF
     C                   ENDIF
     C*
     C* If the E2AACT field contains B or R, route to adjustor
     C*
     C     E2AACT        IFEQ      'B'
     C     E2AACT        OREQ      'R'
     C                   EXSR      RADJ
     C                   ENDIF
     C*
     C* If the E2SACT field contains B or R, route to supervisor
     C*
     C     E2SACT        IFEQ      'B'
     C     E2SACT        OREQ      'R'
     C                   EXSR      RSUP
     C                   ENDIF
     C*
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C*
     C*===== END OF PROCESS
     C*===============================================================*
     C* DADJ - Diary to Adjustor                                      *
     C*===============================================================*
     C     DADJ          BEGSR
     C*
     C*  Find the ADJUSTOR field in the WMCMP file
     C*
     C*
     C                   MOVEL     @@ADJN        QTUSR
     C*
     C*  Diary to ADJUSTOR
     C*
     C                   EXSR      DIARY
     C*
     C     DADJE         ENDSR
     C*===============================================================*
     C* RADJ - Route to Adjustor                                      *
     C*===============================================================*
     C     RADJ          BEGSR
     C*
     C*  Find the ADJUSTOR field in the WMCMP file
     C*
     C*
     C                   MOVEL     @@ADJN        QTUSR
     C*
     C*
      * * * FILL AN ARRAY WITH THE Q ID'S THAT WILL BE PROCESSED.
      *
     C                   MOVE      *BLANKS       QS                             CLEAR SLCTD QS
     C                   MOVEL     QTUSR         QS(1)
     C*
     C*
     C*  Route to ADJUSTOR
     C*
     C                   EXSR      ROUTE
     C*
     C     RADJE         ENDSR
     C*===============================================================*
     C* DSUP - Diary to Supervisor                                    *
     C*===============================================================*
     C     DSUP          BEGSR
     C*
     C*  Find the SUPERVISOR field in the WMCMP file
     C*
     C*
     C*
     C                   MOVEL     @@SUPN        QTUSR
     C*
     C*
     C*  Diary to SUPERVISOR
     C*
     C                   EXSR      DIARY
     C*
     C     DSUPE         ENDSR
     C*===============================================================*
     C* RSUP - Route to Supervisor                                    *
     C*===============================================================*
     C     RSUP          BEGSR
     C*
     C*  Find the SUPERVISOR field in the WMCMP file
     C*
     C*
     C                   MOVEL     @@SUPN        QTUSR
     C*
     C*
      * * * FILL AN ARRAY WITH THE Q ID'S THAT WILL BE PROCESSED.
      *
     C                   MOVE      *BLANKS       QS                             CLEAR SLCTD QS
     C                   MOVEL     QTUSR         QS(1)
     C*
     C*
     C*
     C*  Route to SUPERVISOR
     C*
     C                   EXSR      ROUTE
     C*
     C     RSUPE         ENDSR
     C*===============================================================*
     C* DIARY - Code to diary to new user                             *
     C*===============================================================*
     C     DIARY         BEGSR
     C*
     C*** GET EXISTING DARTS RECORD, THEN UPDATE WITH THE USER THAT NEEDS
     C***  TO BE DIARIED..
     C     QTUSR         IFNE      *BLANKS
     C     CMKYA         CHAIN     PCM04                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     QTUSR         CMUSER
     C                   MOVEL     'Y'           CMDIFL
     C                   MOVEL     Q1PGM         CMPGM
     C*
     C                   UPDATE    PCM04
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C                   ENDSR
     C*===============================================================*
     C* ROUTE - Route to specified user                               *
     C*===============================================================*
     C     ROUTE         BEGSR
     C**********************************************************************
     C* * * READ THROUGH THE QS, AND FOR EACH:
     C*        WRITE A NEW IE RCD.
     C*        SEND A MESSAGE
     C*
     C* * * WHEN FINISHED WITH ALL QS, MARK IT "D"
     C*
     C* * * WE SHOULD ALSO WRITE RCDS TO DOC NOTES FOR EACH NEW ONE AND THE
     C* * * OLD ONE!!!!
     C**********************************************************************
     C*** PROCESS THE REQUESTS ***
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         $$SDT             8 0
     C                   Z-ADD     DSTIME        $$STM             6 0
     C                   MOVEL     *BLANKS       SCMSG1
     C*
     C                   CAT(P)    MDOC:1        SCMSG1
     C                   CAT(P)    DCID:1        SCMSG1
     C                   CAT(P)    MHAS:1        SCMSG1
     C                   CAT(P)    MQUE:1        SCMSG1
     C                   CAT(P)    MID:1         SCMSG1
     C                   CAT(P)    '.  '         SCMSG1
     C*
     C                   MOVEL     *BLANKS       SCMSG2           64
     C*
     C                   CAT(P)    MSEN:1        SCMSG2
     C                   CAT(P)    USRID:1       SCMSG2
     C*
     C                   MOVEL     SCMSG1        ##MSG1           64
     *** BEG ADD ********** 07/09/07 ********************************
     C     DOKY          CHAIN     PDO01                              68
     C     *IN68         IFEQ      *OFF
     C                   Z-ADD     DORCVD        XXRCVD            8 0
     C                   ELSE
     C                   Z-ADD     $$SDT         XXRCVD
     C                   ENDIF
     *** END ADD ********** 07/09/07 ********************************
     C*
     C     IEKY          CHAIN     PIE01                              68
     C  N68              UNLOCK    IMIELH
     C     *IN68         IFEQ      *OFF
     C*** SAVE THE SCAN DATE AND TIME ETC....????
     C                   Z-ADD     1             K                 3 0
     C     QS(K)         DOWNE     *BLANK
     C*
     C                   MOVEL     QS(K)         ##QID
     C*** BYPASS WRITING IF THIS RECORD ALREADY EXISTS IN THE IE FILE
     C     IEKY1         SETLL     PIE01                                  67
     C     *IN67         CABEQ     *ON           BYPWRI
     C*
     C*
     C                   MOVEL     QS(K)         IEQID
     C*
     C                   MOVE      *BLANKS       IESTAT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYMD
     C                   MOVEL     SRDTO         IEIDT
     C                   MOVE      IEIDT         IELDT
     C                   MOVEL     Q1USER        IELUSR
     C                   MOVEL     Q1USER        IEIUSR
     C                   MOVE      DSTIME        IELTIM
     C                   MOVE      DSTIME        IEITIM
     C                   MOVEL     Q1PGM         IELPGM
     C                   MOVEL     Q1PGM         IEIPGM
     ***** ADD 9/20/13 ***********************
     C                   MOVEL     'RL'          IEDSTS
     **** END ADD 9/20/13 ***********************
     *** BEG ADD ********** 07/09/07 ********************************
     C                   Z-ADD     XXRCVD        IERCVD
     *** END ADD ********** 07/09/07 ********************************
     C*
     C                   WRITE     PIE01
     C     BYPWRI        TAG
     C*
     C*** CALL PGM TO SEND MESSAGE
     C***       IN THIS PROGRAM , SCAN FOR &QUEUEID AND IF FOUND REPLACE IT
     C*         WITH THE QUEUE ID THAT IT IS BEING PROCESSED FOR.
     C*
     C     MID           SCAN      ##MSG1        P1                5 0    56
     C     *IN56         IFEQ      *ON
     C                   MOVEA     ##QID         SM(P1)
     C                   ENDIF
     C*
     C                   CALL      'ISSNDMR'
     C                   PARM                    ##QID
     C                   PARM                    SCMSG1
     C                   PARM                    SCMSG2
     C                   PARM      *BLANKS       SCMSG3           64
     C                   PARM      *BLANKS       SCMSG4           64
     C                   PARM      *BLANKS       SCMSG5           64
     C                   PARM      *BLANKS       SCMSG6           64
     C                   PARM      *BLANKS       SCMSG7           64
     C                   PARM      *BLANKS       SCMSG8           64
     C                   PARM      'Y'           SCBRKM            1
     C                   PARM                    @$RTN             1
     C*
     C                   EXSR      DNOT
     C*
     C* * * CALL EXIT PROGRAM FOR REASSIGNS
     C*
     C                   CALL      'ISE001C'                            99      NO ERR
     C                   PARM                    QQINQ            10            FROM QUEUE
     C                   PARM                    ##QID            10            TO   QUEUE
     C                   PARM                    Q1USER                         FROM USER
     C                   PARM                    QQDCID           12            DOC ID
     C                   PARM                    XRTCD             6            XIT RTN CD
     C*
     C                   ADD       1             K
     C*
     C                   ENDDO
     C*** MARK IT AS "D"ONE....  (NO NEED AS IS210R DOES THIS)`
     C*****      IEKY      CHAINPIE01                68
     C*****      *IN68     IFEQ *OFF
     C*****                MOVE 'D'       IESTAT
     C*
     C*****                TIME           DSDTTM
     C****       10000.01  MULT DSDATE    IELDT
     C****8                MOVELQ1USER    IELUSR
     C****                 MOVE DSTIME    IELTIM
     C****                 MOVELQ1PGM     IELPGM
     C*
     C*****                UPDATPIE01
     C*****                ENDIF
     C*
     C                   ENDIF
     C                   ENDSR
     C*====================================================================*
     C* * *   DNOT DOC NOTES  - CREATE DOC NOTES AND CPY NOTES TO SMAR* * **
     C*====================================================================*
     C     DNOT          BEGSR
     C*
     C*** FILL ALL DOC NOTE FIELDS
     C** GET CURRENT DATE/TIME TO CREATE NOTES...
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        #$NOWD            8
     C                   MOVE      DSTIME        #$NOWT            6
     C*
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
     C*
     C     DDAT          CAT(P)    XXMDYY:1      XX75
     C                   CAT(P)    DTIM:1        XX75
     C                   CAT(P)    XXHMS:1       XX75
     C                   CAT(P)    DREA:5        XX75
     C*
     C                   MOVEL     XX75          DN(1)
     C*
     C*
     C     MDOC          CAT(P)    QQDCID:1      XX75
     C                   CAT(P)    DHBR:1        XX75
     C                   CAT(P)    DTOQ:1        XX75
     C                   CAT(P)    ##QID:1       XX75
     C                   CAT(P)    '.'           XX75
     C*
     C                   MOVEL     XX75          DN(2)
     C*
     C*
     C     MSEN          CAT(P)    Q1USER:1      XX75
     C                   CAT(P)    '.'           XX75
     C                   MOVEL     XX75          DN(3)
     C                   MOVEL     *BLANKS       DN(4)
     C*
     C*
     C*** CALL PGM TO WRITE DOC NOTES
     C*
     C* * * DOW LOOP HERE TO PROCESS MULTIPLE ITEMS.  SETUP ITEMS
     C* * * AS COMMENTS, OR ARRAY ELEMENTS
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
     C*****************    CALL 'ISNOTAR'
     C***                  PARM           QQDCID 12
     C***                  PARM PIKEY     Q@KEY  40
     C***                  PARM PISEQ#    Q@SEQ#  50
     C*****************    PARM *BLANKS   Q@RCD   1
     C*
     C*
     C                   ENDSR
