     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IS215R1 - INDEXING                                       *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*  40    GENERAL CHAIN                                          *
     H*  60    GENERAL READ IND.                                      *
     H*****************************************************************
     H* RETURN CODES :
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                                   *
     H* 04/10/19 C:062 Project: 152WAB                                    *
     H*                Apply CSI mods for S1 Medical.                     *
     H*
     H*****************************************************************
     H* COMMENTED SY195 CHNAGES AS PROJECT IS IN DVP.
     H*****************************************************************
     H*                                                               *
     H*  12/05/11 C:190 Project: 114ICM                               *
     H*                 Make sure documents are retained on the       *
     H*                 users document queue when it is re-indexed to *
     H*                 a claim with the same user assigned, if       *
     H*                 CLTID = 114.                                  *
     H*                                                               *
     H*  10/17/11 C:279 Project: SY195
     H*                 Modify program to pass/return UNC path field
     H*                 and IFS flag to called/calling programs.
     H*
     H*** 1/3/07  C: 02 AOOPTICAL
     F**                REMOVE DATACENTER PROCESSING (PREFIX) AS IT
     F**                SHOULD NOT BE USED IN THIS PROGRAM.
     F****
     H* 06/18/04 C:062 Project SY100                                  *
     H*                Add Data Center Processing                     *
     H*                                                               *
     H* 04/12/04 C:062 Project 114MEX                                 *
     H*                Added assign pgm process.                      *
     H*                                                               *
     H* 04/01/04 C:62 PROJECT 114MEX                                  *
     H*               IF NOT KEPT ON QUEUE THEN GIVE OPTION TO ASSIGN *
     H*               IF THE DOCUMENT TYPE IS PIRUS                   *
     H*                                                               *
     H* 06/12/03 C:62 PROJECT 114MEX                                  *
     H*               IF NOT KEPT ON QUEUE THEN GIVE OPTION TO ASSIGN *
     H*               IF THE DOCUMENT TYPE IS RUSHBILL                *
     H*                                                               *
     H* 04/01/03 C:62 PROJECT 114MEX                                  *
     H*               IF NOT KEPT ON QUEUE THEN GIVE OPTION TO ASSIGN *
     H*                                                               *
     H*****************************************************************
      *
      ***FROM: IDXPGM (ISII01-99 SERIES)-
      *
      *          "C"   F12 PRESSED                           *CMDKEY
      *          "3"   F3  PRESSED                           *CMDKEY
      *
      ***TO:   CALLING PROGRAM
      *
      *          "L"   RCD LOCKED BY SOMEONE ELSE      -     *ERROR
      *          "C"   F12 PRESSED FROM IDXPGM               *CMDKEY
      *          "3"   F3  PRESSED FROM IDXPGM               *CMDKEY
      *          "R"   RETAIN RCD ON QUEUE AFTER INDEX!      *PROCESS
      *
     H*****************************************************************
      * REVISIONS
      *
      * 01/27/00 C:113 PROJECT: DR095                                 *
      *                Changed to use the Work With Key Types program *
      *                (SC092R) for key type searches.                *
      *                                                               *
      * 01/04/00 C:113 PROJECT: SUPPORT
      *                Corrected date conversion.
      *
      *****************************************************************
     F*
     FIMDTL     IF   E           K DISK
     FIMIELF    UF   E           K DISK
     F                                     INFDS(FILDS1)
     FIMIQL     IF   E           K DISK
     FIMDOL1    UF   E           K DISK
     FIMBML     UF   E           K DISK
     FIMDOL4    IF   E           K DISK
     F                                     RENAME(PDO01:PDO04)
     FIMIELH    UF   E           K DISK
     F                                     RENAME(PIE01:PIE06)
     FIMDOP1    O    E             DISK
     FIMIELC    IF   E           K DISK
     F                                     RENAME(PIE01:PIE0C)
     F*** BEGIN ADD *** 01/27/00 ***********************************************
     F*
     F** Key Type File
     FSMKTP     IF   E             DISK
     F*** END   ADD *** 01/27/00 ***********************************************

     D*
     D                 DS
     D*
     D  IESDT                  1     08  0
     D  XIEY                   1      4  0
     D  XIEMD                  5      8  0
     D*
     D                 DS
     D*
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D FILDS1          DS
     D  FIL1             *FILE
     D  REC1             *RECORD
     D  OP1              *OPCODE
     D  STS1             *STATUS
     D  RTN1             *ROUTINE

     I*

     C*
     C*   KEY LIST FOR THE DOCUMENT TYPE  FILE
     C*
     C     DTKY          KLIST
     C                   KFLD                    $$DOCT
     C*
     C*
     C*   KEY LIST FOR THE IMAGE QUEUE  FILE
     C*
     C     IQKY          KLIST
     C                   KFLD                    $$QID
     C*
     C*
     C*   KEY LIST FOR THE INDEXING FILE
     C*
     C     IEKY          KLIST
     C                   KFLD                    $$QID
     C                   KFLD                    $$DCID           12
     C                   KFLD                    ##SDT             8 0
     C                   KFLD                    ##STM             6 0
     C*
     C*
     C*
     C*
     C*
     C     IEKY1         KLIST
     C                   KFLD                    $$QID
     C                   KFLD                    SCDCID
     C*
     C*
     C*   KEY LIST FOR THE DOCUMENT TRANSACTION FILE
     C*
     C     DOKY          KLIST
     C                   KFLD                    SCDCID
     C*
     C*
     C*
     C     BMKY1         KLIST
     C                   KFLD                    DOBCHI
     C*
     C*   KEY LIST FOR THE DOCUMENT TRANSACTION FILE
     C*
     C     DOKY4         KLIST
     C                   KFLD                    BMBCHI
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8            MDY(YY)
     C                   PARM      '*YYMD'       SRTFMT            8            YYMD
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
      *** BEGIN ADD *** 01/04/00 ***********************************************
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *** END   ADD *** 01/04/00 ***********************************************
     C*** BEGIN ADD *** 01/27/00 ***********************************************
     C     ACDPL1        PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      '2'           ##VIEW            1
     C*** END   ADD *** 01/27/00 ***********************************************
     C*** BEGIN ADD *** 04/12/04 **************************************
     C     ASSPL         PLIST
     C     $$QID         PARM      $$QID         $#QID            10            10A Q ID
     C                   PARM      *BLANKS       $#USER           10            10A USER
     C                   PARM      IESDT         $#SDT             8 0           80 SCNDTMDY
     C                   PARM      IESTM         $#STM             6 0           60 SCTIM
     C                   PARM      IEDCID        $#DCID           12            12A DCID
     C                   PARM      *BLANKS       $#DESC           30            30A DC DSC
     C                   PARM      IEDOCT        $#DOCT            8             8A DOCTYP
     C                   PARM      IEFL01        $#FL01            1             1A IMG/FAX
     C                   PARM      *BLANKS       $#PTH            63            63A FULL PTH
     C                   PARM      *BLANKS       $#FLR             8             8A FOLD
     C                   PARM      *BLANKS       $#SFLR           12            12A SUBFLR
     C     QQRTCD        PARM      QQRTCD        $#RTCD            1             1A RTNCODE
     C*** END   ADD *** 04/12/04 **************************************
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    $$QID            10
     C                   PARM                    $$DCID           12
     C                   PARM                    $$SDT             8
     C                   PARM                    $$STM             6
     C                   PARM                    $$DOCT            8
     C                   PARM                    Q@RTC             1
     C                   MOVEL     Q@RTC         #@RTC             1
     C                   MOVEL     *BLANKS       Q@RTC
     C*
     C*** BEG ADD *** 12/05/11 ****************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*** END ADD *** 12/05/11 ****************************************
     C                   TIME                    DSDTTM
     C*
     C*
     C                   MOVE      $$STM         ##STM
     C*
     C*** COVERT DATE TO YYYYMMDD FORMAT FOR FILE CHAINS ETC.
     C*
     C                   MOVE      $$SDT         SRDTI
     C                   CALL      'ISCVTDT'     DTYYMD
     C                   MOVEL     SRDTO         ##SDT
     C*
     C*
     C*
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C     TOP           TAG
     C*
     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
     C*
     C     STRT          TAG
     C*
     C     VWRQS         TAG
     C*
     C     DTKY          CHAIN     PDT01                              60
     C*
     C                   MOVEL     DTDOCT        SCDOCT
     C                   MOVEL     DTDESC        SCDDSC
     C******************** MOVELDTDQUE    $$QID
     C                   MOVEL     *BLANKS       IDXPGM
     C                   MOVEL     DTIDXP        IDXPGM           10            INDEX PGM
     C*** BEGIN ADD *** 04/12/04 **************************************
     C                   MOVEL     DT10A1        ASSPGM           10            ASSIGN PGM
     C*** END   ADD *** 04/12/04 **************************************
     C*
     C     IQKY          CHAIN     PIQ01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     IQDESC        SCQDSC           40
     C                   ELSE
     C                   MOVEL     *BLANKS       SCQDSC
     C                   ENDIF
     C*
     C*
     C     BMKY1         CHAIN     PBM01                              60
     C     *IN60         IFEQ      *OFF
     C                   UNLOCK    IMBML
     C                   MOVEL     BMBDSC        SCBDSC           40
     C                   ELSE
     C                   MOVEL     *BLANKS       SCBDSC
     C                   ENDIF
     C*
     C****  GET THE RECORD TO PROCESS HERE ***
     C*
     C     IEKY          SETLL     PIE01                              60
     C     NXTIE         TAG
     C                   MOVEL     *BLANKS       IDXPGM
     C                   MOVEL     DTIDXP        IDXPGM           10            INDEX PGM
     C     IEKY          READE     PIE01                                  60
     C     *IN60         IFEQ      *OFF
     C*** IF WE GET A HIT , THIS RCD IS AVAIL. AND NOT LOCKED BY ANOTHER USE
     C***   USER, IF NOT, THEN WE CAN ASSUME IT IS LOCKED BY ANTOHER USER.
     C*
     ************IESTAT    CABEQ'L'       NXTIE************************
     C*
     C                   MOVE      'L'           IESTAT                          LOCK THE RCD!!!
     C                   UPDATE    PIE01
     C*
     C                   MOVEL     IEDCID        SCDCID           12
     C*
     C********* SCSDAT IS MDY ****
     C*
      *** BEGIN CHG *** 01/04/00 ***********************************************
     C********** XIEMD     MULT 10000     SCSDAT
     C**********           ADD  XIEY      SCSDAT
     C                   Z-ADD     IESDT         SRDTI
     C                   CALL      'ISCVTDT'     DTMDYY
     C                   MOVEL     SRDTO         SCSDAT
      *** END   CHG *** 01/04/00 ***********************************************
     C                   MOVE      IESTM         SCSTIM
     C                   MOVEL     IESREP        SCSREP            8
     C*
     C                   ELSE
     C* * * RECORD HAS BEEN LOCKED BY ANOTHER USER, RETURN TO CALLING PGM
      * * *  WITH A RTN CODE THAT SAYS THIS....
     C                   MOVE      *ZEROS        SCSDAT
     C                   MOVE      *ZEROS        SCSDAT
     C                   MOVE      *ZEROS        SCSTIM
     C                   MOVEL     *BLANKS       SCSREP
     C*
     C                   MOVE      'L'           Q@RTC                           "L" IS RCD LCKD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C***
     C     DOKY          CHAIN     PDO01                              60
     C     *IN60         IFEQ      *OFF
     C                   UNLOCK    IMDOL1
     C*
     C                   MOVEL     DOFLR         XXFLR             8
     C                   MOVEL     DOSFLR        XXSFLR           12
     C     XXFLR         CAT       '/'           XX9A              9
     C*** BEG ADD *** 10/17/11 ****************************************
     C                   MOVE      DOUNCN        QQUNCN          255
     C                   MOVE      DOIFSP        QQIFSP            1
     C*** END ADD *** 10/17/11 ****************************************
     C*** BEGIN ADD *** 06/18/04 **************************************
     C***** DEL 1/3/07 ***********************************************
     C***                  CALL 'SA030R'
     C***                  PARM *BLANKS   ##PRFX  3
     C*********            PARM *BLANKS   ##PFLR 80
     C*
     C***        ##PFLR    IFNE *BLANKS
     C***        ##PFLR    CAT  '/':0     SCPTH     P
     C***                  CAT  XX9A:0    SCPTH
     C***                  CAT  XXSFLR:0  SCPTH
     C*** DEL 1/3/07 ***   ELSE
     C*** END   ADD *** 06/18/04 **************************************
     C     XX9A          CAT       XXSFLR        SCPTH
     C*** BEGIN ADD *** 06/18/04 **************************************
     C***** DEL 1/3/07 ***********************************************
     C**** DEL             ENDIF
     C*** BEGIN ADD *** 06/18/04 **************************************
     C*
     C                   MOVE      DOPGS         SC#PGS
     C                   MOVEL     DOBCHI        SCBCHI
     C                   ELSE
     C                   MOVEL     *BLANKS       SCPTH
     C*
     C                   MOVE      *ZEROS        SC#PGS
     C                   MOVEL     *BLANKS       SCBCHI
     C*** BEG ADD *** 10/17/11 ****************************************
     C                   MOVE      *BLANKS       QQUNCN          255
     C                   MOVE      *BLANKS       QQIFSP            1
     C*** END ADD *** 10/17/11 ****************************************
     C                   ENDIF
     C*
     C*
      *
      *** CALL PROGRAM TO PROCESS THE VIEW REQUEST
      *
     C     IEFL01        IFEQ      'F'                                          FAX DOC
     C*
     C*
     C*** CALL THE DSPFAXC PROGRAM TO DISPLAY THE FAX, ELSE
     C                   CALL      'DSPFAXC'
     C                   PARM                    SCDCID           12
     C                   PARM      SCPTH         $$FOLD           80
     C                   PARM      *BLANKS       Q@RTRN            1
      *
     C                   ELSE
     C*** IMAGE DISPLAY
     C                   MOVEL     *BLANKS       QQRTN
     C*
     C                   CALL      'IS211C'
     C                   PARM                    SCDCID           12
     C                   PARM                    XXFLR             8
     C                   PARM                    XXSFLR           12
     C                   PARM                    SCDOCT
     C                   PARM                    SCDDSC
     C                   PARM                    QQRTN             6
      *
      *
      *
      *
     C                   ENDIF                                                    -IMAGE
      *
      *
      *** CALL INDEX PROGRAM TO PERFORM INDEX OF THE DOCUMENT
      *
     C     IDXDSP        TAG
      *
     C                   CALL      IDXPGM
     C                   PARM                    SCDCID           12
     C                   PARM                    SCPTH            40
     C                   PARM                    SCDOCT            8
     C                   PARM                    SCDDSC           40
     C                   PARM                    $$QID            10
     C                   PARM                    SCQDSC           40
     C                   PARM                    SCBCHI           12
     C                   PARM                    SC#PGS            6 0
     C                   PARM                    SCSDAT            8 0
     C                   PARM                    SCSTIM            6 0
     C                   PARM                    SCSREP            8
     C                   PARM                    XXKEY            40
     C                   PARM                    XXSEQ#            5 0
     C                   PARM      #@RTC         QQRTCD            1
     C*** BEG ADD *** 10/17/11 ****************************************
     C                   PARM                    QQUNCN          255
     C                   PARM                    QQIFSP            1
     C*** END ADD *** 10/17/11 ****************************************
     C*** BEG ADD *** 12/05/11 ****************************************
     C*** Begin Chg *** 04/10/19 *************************************
     C*****XXCLT#        IFEQ      '114'
     C*****QQRTCD        ANDEQ     *BLANKS
     C**********         MOVEL     'R'           QQRTCD
     C**********         ENDIF
     C     XXCLT#        IFEQ      '114'
     C     XXCLT#        OREQ      '152'
     C     QQRTCD        IFEQ      *BLANKS
     C                   MOVEL     'R'           QQRTCD
     C                   ENDIF
     C                   ENDIF
     C*** End   Add *** 04/10/19 *************************************
     C*** END ADD *** 12/05/11 ****************************************
      *
      **** PROCESS THE COMMAND KEYS 1ST!!!!
      *** WE MUST UNLOCK THE IE RECORD IF WE DONT USE IT!!!!!
      *
      ******** UPDATE THE DO RCD AND THE IE RCD THEN TRY FOR ANOTHER!!!!
      *
      * * * GO AGAIN.... NEXT DOC TO BE INDEXED FOR THE PRIOR SELECTED
      * * * DOC TYPE.
      *
      *** UNLOCK  RCDS ETC.
      *
     C     QQRTCD        IFEQ      *BLANK
&&&  C     QQRTCD        OREQ      'R'
     C     XXKEY         ANDNE     *BLANK
     C*** BEGIN ADD *** 04/12/04 **************************************
     C                   MOVEL     $$QID         HDQID            10
     C                   CALL      ASSPGM        ASSPL                  99
     C*** END   ADD *** 04/12/04 **************************************
     C*
     C     IEKY1         CHAIN     PIE06                              60
     C     *IN60         IFEQ      *OFF
     C*** BEGIN ADD *** 04/12/04 **************************************
     C                   MOVEL     HDQID         $$QID
     C*** END   ADD *** 04/12/04 **************************************
&&&  C     QQRTCD        IFNE      'R'
     C                   MOVE      'D'           IESTAT
     C                   MOVE      'CM'          IEDSTS
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      XXDT              6 0
     C                   Z-ADD     XXDT          IELDT
     C                   Z-ADD     DSTIME        IELTIM
     C                   MOVEL     Q1USER        IELUSR
     C                   UPDATE    PIE06
&&&  C                   ENDIF
     C*
     C*** WE NOW CALL THE SUB PGM 125R2 TO UPDATE/REMOVE THE ORIG. SCAN
     C**** RECORD IF IT EXISTS. IE RECORD    (DSTS=HD, STAT<>D)
     C*
     C                   CALL      'IS215R2'
     C                   PARM      IEDCID        X@DCID           12
     C                   PARM      *BLANKS       X@RTCD            1
     C*
     C*
      * * * UPDATE THE DO RECORD WITH THE DARTS KEY AND FLAG ETC.
     C     DOKY          CHAIN     PDO01                              60
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVEL     XXKEY         DODKEY
     C                   MOVE      XXSEQ#        DODSQ#
     C                   MOVEL     'Y'           DOIDFX
     C*** STATUS IS ONLY UPDATED TO "CM" IF ALL INDEXES ARE COMPLETE
     C     DOKY          SETLL     PIE0C                                  61    61=RCD FOUND
     C     *IN61         IFEQ      *OFF                                         IF ALL ARE IDXD
     C*
     C                   MOVEL     'CM'          DODSTS
     C*
     C                   ELSE
     C*
     C*
     C                   ENDIF
     C                   UPDATE    PDO01
     C*** CREATE AN INDEX RECORD HERE****
     C     XXKEY         IFNE      *BLANKS
     C                   WRITE     PDO011
     C                   ENDIF
     C*
     C* * *  PERFORM DOC NOTES   HERE!!!  ????? DONE IN THE INDEX PGM???
     C*
     C                   EXSR      BCHUPD
     C*
     C                   ENDIF
      *
     C                   ENDIF
      *
      *
     C                   ELSE
      *
      **** ALLOW A F KEY TO RETURN AND ALLOW SELECT OF OTHER DOC TYPE.
      *
     C     QQRTCD        IFEQ      '3'
     C     QQRTCD        OREQ      'C'
     C     QQRTCD        OREQ      'P'
      *
     C     IEKY1         CHAIN     PIE06                              60
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      ' '           IESTAT
     C                   UPDATE    PIE06
      *
     C                   ENDIF
     C*
     C*** ON RTRN WITH F3(3) OR F12(C) , NEED TO RETURN TO CALLING PGM WITH
     C* RETRN CODE INFO...
     C                   MOVEL     QQRTCD        Q@RTC
     C                   GOTO      TERM
      *
     C*
     C                   ELSE
     C*
      *** IF THE PGM RTNS A RQS FOR A DIFFERENT KEY PROGRAM (INDEXPGM)
     C     QQRTCD        IFEQ      'K'
      **
     C                   EXSR      NEWIDX
      **
     C                   ENDIF
     C*
     C                   MOVEL     QQRTCD        Q@RTC
     C                   ENDIF
     C*
     C                   ENDIF
      *
     C     IEKY1         CHAIN     PIE06                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ' '           IESTAT
     C                   UPDATE    PIE06
     C                   ENDIF
     C*
     C*******************  GOTO NXTIE
     C*
     C     TERM          TAG
     C                   RETURN
     C*
     C*** END OF PROGRAM
     C     ENDPGM        TAG
     C                   MOVE      '1'           *INLR
     C*
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C     INIT          BEGSR
     C*
     C*
     C                   ENDSR
     C*
      *====================================================================*
      * * *   NEWIDX SUBROUTINE                              N E W I D X ***
      *====================================================================*
     C     NEWIDX        BEGSR
     C*
      * * * DISPLAY LIST OF INDEX TYPES TO CHOOSE.
      *
      *** BEGIN DEL *** 01/27/00 ***********************************************
     C**********           CALL 'IS213R'
     C**********           PARM *BLANKS   QXIPGM 10
     C**********           PARM *BLANKS   QXRTCD  1
     C**********
     C**********           MOVELQXIPGM    IDXPGM
      *** END   DEL *** 01/27/00 ***********************************************
     C*** BEGIN ADD *** 01/27/00 ***********************************************
     C                   CALL      'SC092R'      ACDPL1
     C*
     C     *INKC         IFEQ      '1'
     C     *INKL         OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
      *
     C     IEKY1         CHAIN     PIE06                              60
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      ' '           IESTAT
     C                   UPDATE    PIE06
      *
     C                   ENDIF
     C*
     C*** ON RTRN WITH F3(3) OR F12(C) , NEED TO RETURN TO CALLING PGM WITH
     C* RETRN CODE INFO...
     C     *INKC         IFEQ      '1'
     C                   MOVEL     '3'           Q@RTC
     C                   ELSE
     C     *INKL         IFEQ      '1'
     C                   MOVE      'C'           Q@RTC
     C                   ELSE
     C                   MOVE      'P'           Q@RTC
     C                   ENDIF
     C                   ENDIF
     C                   GOTO      TERM
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PKT01                              60
     C                   MOVEL     KTINPG        IDXPGM
     C                   ENDIF
     C*
     C*** END   ADD *** 01/27/00 ***********************************************
     C                   GOTO      IDXDSP
     C*
     C                   ENDSR
      *====================================================================*
     C*
      *====================================================================*
      * * *   BCHUPD SUBROUTINE                              B C H U P D ***
      *====================================================================*
     C     BCHUPD        BEGSR
     C*
     C     BMKY1         CHAIN     PBM01                              60
     C     *IN60         IFEQ      *OFF
     C     BMBSTS        ANDEQ     'RL'
     C*
     C     DOKY4         CHAIN     PDO04                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     'RL'          BMBSTS
     C                   ELSE
     C                   MOVEL     'CM'          BMBSTS
     C* * *  PERFORM BATCH NOTES HERE!!!
     C                   ENDIF
     C*
     C*
     C                   ENDIF
     C  N60              UPDATE    PBM01
     C*
     C                   ENDSR
     C*                                                                     ****
