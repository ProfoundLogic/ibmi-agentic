     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IS420R - PROCESS ALL SCAN/LB RCDS TO LOAD BAL SCANNED DOCS*
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     F* 07/09/07 C:191 PROJECT: 10IJI                                 *
     F*               -WRITE RECEIVED DATE TO IMIEP                   *
     H*                                                               *
     H* 05/28/08 C:062 PROJECT: 62718                                 *
     H*                CHANGE TIME UPDATE OF PDL01                    *
     H*                                                               *
     H* 07/09/99 C:113 PROJECT: DR076                                 *
     H*                Changed to use new 7/0 date field in IMDLP.    *
     H***                                                             *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H*  60    GENERAL CHAIN INDICATORE                               *
     H*****************************************************************
     FIMSLL2    IF   E           K DISK
     FIMIELJ    UF   E           K DISK
      *** BEGIN CHG ************************************************************
     F*****IMDLL2  UF  E           K        DISK
     FIMDLL4    UF   E           K DISK
      *** END   CHG ************************************************************
     FIMIEL6    UF   E           K DISK
     F                                     RENAME(PIE01:PIE06)
     FIMDTL     IF   E           K DISK
     FIMIEP     O    E             DISK
     F                                     RENAME(PIE01:PIE0P)
     F*** BEG ADD ********** 07/09/07 ********************************
     F*** LOGICAL OF IMDOP
     FIMDOL1    IF   E           K DISK

     D*** END ADD ********** 07/09/07 ********************************
      *=========================================================================
      *  Table and Array Definitions
      *=========================================================================
     D DN              S             75    DIM(4)                               DOCN's
      *=========================================================================
      *  Data Structure and Constant Definitions
      *=========================================================================
     D NOLNK           C                   CONST('No queues available ')
     D MDOC            C                   CONST('Document:')
     D MHAS            C                   CONST('has arrived on')
     D MQUE            C                   CONST('Queue:')
     D MSEN            C                   CONST('Sent by:')
     D MID             C                   CONST('&QUEUE  ID')
     D****
     D DDAT            C                   CONST('Date:')
     D DTIM            C                   CONST('Time:')
     D DREA            C                   CONST('LOAD BAL')
     D DHBR            C                   CONST('has been reassigned')
     D DTOQ            C                   CONST('to Queue:')
     D*
     D                 DS
     D  SCMSG1                 1     64
     D  SM                     1     64
     D                                     DIM(64)                              SCMSG
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
      ** External data structures
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *=========================================================================
      *  PLIST Definitions
      *=========================================================================

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
      *** BEGIN ADD *** 07/09/99 ***********************************************
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8            MDYY
     C                   PARM      '*CYMD'       SRTFMT            8            CYYMMDD
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
      *** END   ADD *** 07/09/99 ***********************************************
      *=========================================================================
      *  KLIST Definitions
      *=========================================================================
     C     DTKY          KLIST
     C                   KFLD                    SLDOCT
     C*
     C     DTKY1         KLIST
     C                   KFLD                    SLDOCT
     C*
     C*   KEY LIST FOR THE IMAGE INDEXING FILE
     C*
     C     IEKY          KLIST
     C                   KFLD                    QQQID
     C*
     C*
     C     IEKY1         KLIST
     C                   KFLD                    QQQID
     C                   KFLD                    QQDCID           12
     C*
     C*
     C     DLKY          KLIST
     C                   KFLD                    SLID
     C*
     C     DLKY1         KLIST
     C                   KFLD                    SLID
      *** BEGIN CHG *** 07/09/99 ***********************************************
     C******************** KFLD           ##DT    60
     C                   KFLD                    ##DT              7 0
      *** END   CHG *** 07/09/99 ***********************************************
     C                   KFLD                    ##TM              6 0
     *** BEG ADD ********** 07/09/07 ********************************
     C     DOKY          KLIST
     C                   KFLD                    QQDCID
     *** END ADD ********** 07/09/07 ********************************
      *=========================================================================
      *  Main
      *=========================================================================
     C                   TIME                    DSDTTM
     C*
     C*** BEGIN ADD *** 05/28/08 **************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0          CYYMMDD
     C                   MOVE      DSTIME        XXTM              6 0
     C*** END   ADD *** 05/28/08 **************************************
     C*** BEGIN DEL *** 05/28/08 **************************************
     C**********           Z-ADD*ZEROS    ##DT
     C**********           Z-ADD*ZEROS    ##TM
     C*** END   DEL *** 05/28/08 **************************************
     C     *LOVAL        SETLL     PSL01
     C                   READ      PSL01                                  63
     C     *IN63         DOWEQ     *OFF
     C*
     C*** FOR EVERY SCAN/LB RECORD, GO TO QUEUE, READ EACH ENTRY
     C*** FOR EACH ENTRIE IN QUEUE, USE THE DIST/LB ID TO SELECT THE NEXT
     C***   USER QUEUE TO ROUTE TO, AND PERFORM THE ROUTING.
     C*** THEN READ NEXT ITEM IN DOCTYPE QUEUE,  THEN DO THE SAME
     C***   WHEN ALL PROCESSED FOR THE DOC TYPE QUEUE, GO TO THE NEXT ENTRY
     C****   IN THE SCAN/LB FILE AND LOOP THROUGH ALL ENTRIES IN IT'S
     C***    QUEUE.
     C*
     C*** NOW DO LOOP FOR ALL RCDS IN THE DOCTYPE QUEUE
     C*
     C*
     C     DTKY          CHAIN     PDT01                              62
     C*
     C                   MOVEL     DTDQUE        QQQID            10
     C*** BEGIN ADD *** 05/28/08 **************************************
     C                   MOVE      *ON           FIRST             1
     C*** END   ADD *** 05/28/08 **************************************
     C*
     C*
     C     IEKY          SETLL     PIE01                              62
     C     NXTIE         TAG
     C     IEKY          READE     PIE01                                  62
     C     *IN62         IFEQ      *OFF
     C     IESTAT        CABEQ     'L'           NXTIE
     C                   MOVE      'L'           IESTAT
     C                   UPDATE    PIE01
     C*
     C*
     C*** THIS IS WHERE WE GET THE LB QUEUE FROM THE DIST/LB FILE
     C***
      *//////////////////////////////////////////////////////////////////
      * WE MUST * LEAVE THE IE RCD AS IS EXCPT WE MUST CHANGE THE DOC STS
      *  TO BE HD (HELD?).  THIS IS SO THE RCD DOES NOT GET DELETED.
      *  ALL RCDS MUST BE INDEXED OR DELETED/REJECTED FOR IMAGES.
      *
      *  WE MUST MODIFY THE USER QUEUE PGMS TO  VERIFY THAT THE IMAGE
      *  IS STILL ON A QUEUE OF SOME SORT BEFORE WE ALLOW A REMOVE. WE CAN
      *  THEN CHG THE ORIGINALS STATUS TO RL IF THERE ARE NO MORE ON A QUEUE
      *  IF THE ITEM IS TO BE INDEXED, WE CAN "CM" THE ORIGINAL RECORD.
      *//////////////////////////////////////////////////////////////////
     C*
     C                   MOVE      IEDCID        QQDCID
     *** BEG ADD ********** 07/09/07 ********************************
     C     DOKY          CHAIN     PDO01                              68
     C     *IN68         IFEQ      *OFF
     C                   Z-ADD     DORCVD        XXRCVD            8 0
     C                   ELSE
     C                   Z-ADD     0             XXRCVD
     C                   ENDIF
     *** END ADD ********** 07/09/07 ********************************
     C*
     C** THEN WE DO THE "REASSIGN" FUNCTION.
     C*
     C*** BEGIN ADD *** 05/28/08 **************************************
     C*
     C*  IF THIS IS THE FIRST ITEM PROCESSED IN THE QUEUE THEN LET'S
     C*  FIND OUT IF THE CURRENT DATE/TIME IS LESS THAN THE GREATER
     C*  MAINTENANCE DATE.  THIS IS AN ISSUE SINCE WE ARE USING THE
     C*  MAINTENANCE TIME AS A SEQUENCE NUMBER.
     C*
     C     FIRST         IFEQ      *ON
     C                   Z-ADD     *HIVAL        ##DT
     C                   Z-ADD     *HIVAL        ##TM
     C     DLKY1         SETGT     PDL01
     C     DLKY          READPE    PDL01                                  61
     C     *IN61         IFEQ      *OFF
     C     DLLDAT        IFEQ      XXDT
     C     DLLTIM        ANDGT     XXTM
     C     DLLDAT        ORGT      XXDT
     C                   Z-ADD     DLLDAT        XXDT
     C                   Z-ADD     DLLTIM        XXTM
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      *OFF          FIRST
     C                   ENDIF
     C*
     C                   Z-ADD     *ZEROS        ##DT
     C                   Z-ADD     *ZEROS        ##TM
     C*** END   ADD *** 05/28/08 **************************************
     C     DLKY1         SETLL     PDL01
     C     DLKY          READE     PDL01                                  61
     C*
     C     *IN61         IFEQ      *OFF
     C*** NOW USE THE DLIMGQ TO RE ASSIGN THE DOC
     C*
     C                   EXSR      PRCSS
     C*
     C*** BEGIN DEL *** 05/28/08 **************************************
     C**********           TIME           DSDTTM
      *** BEGIN CHG *** 07/09/99 ***********************************************
     C********** DSDATE    MULT 10000.01  XXDT    60       YY/MM/DD
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXDT    70       CYYMMDD
      *** END   CHG *** 07/09/99 ***********************************************
     C*
     C**********           MOVE DSTIME    XXTM    60
     C********** XXTM      IFEQ HLDTM
     C*** END   DEL *** 05/28/08 **************************************
     C                   ADD       000001        XXTM
     C*** BEGIN DEL *** 05/28/08 **************************************
     C**********           ENDIF
     C*** END   DEL *** 05/28/08 **************************************
     C*
      *** BEGIN CHG *** 07/09/99 ***********************************************
     C******************** Z-ADDXXDT      DLLDT              LAST MAINT DT.
     C                   Z-ADD     XXDT          DLLDAT                           LAST MAINT DT.
      *** END   CHG *** 07/09/99 ***********************************************
     C                   Z-ADD     XXTM          DLLTIM                           LAST MAINT TM.
     C*** BEGIN DEL *** 05/28/08 **************************************
     C**********           Z-ADDXXTM      HLDTM   60         LAST MAINT TM.
     C*** END   DEL *** 05/28/08 **************************************
     C*
     C                   UPDATE    PDL01
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       QQDCID
     C***
     C*
     C*
     C*
     C*
     C                   GOTO      LEEV
     C                   ENDIF
     C*
     C*** HERE WE ACTUALLY UPDATE THE ORIGINAL RCD WITH A HD STS AND " "
     C***  AS THE RCD STATUS.  THIS IS TO ALLOW THE RCD TO BE RETAINED
     C***  BUT NOT SEEN IN ANY NORMAL QUEUES.  I WILL NEED TO HAVE
     C***  THIS RCD UPDATED LATER IF AN INDEX HAPPENS, OR IF
     C***  THERE IS AN ATTEMPT TO DELETE IT!!!!
     C*
     C     IEKY1         CHAIN     PIE06                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ' '           IESTAT
     C                   MOVEL     'HD'          IEDSTS
     C*
     C                   TIME                    DSDTTM
     C     10000.01      MULT      DSDATE        IELDT
     C                   MOVEL     Q1USER        IELUSR
     C                   MOVE      DSTIME        IELTIM
     C                   MOVEL     Q1PGM         IELPGM
     C*
     C                   UPDATE    PIE06
     C                   ENDIF
     C*
     C                   GOTO      NXTIE
     C*
     C     LEEV          TAG
     C*
     C*
     C                   READ      PSL01                                  63
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
      *====================================================================*
      * * *   PRCSS SUBROUTINE
      *====================================================================*
     C     PRCSS         BEGSR
     C*
     C*
     C                   MOVEL     *BLANKS       SCMSG1
      *
     C     MDOC          CAT(P)    IEDCID:1      SCMSG1
     C                   CAT(P)    MHAS:1        SCMSG1
     C                   CAT(P)    MQUE:1        SCMSG1
     C                   CAT(P)    MID:1         SCMSG1
     C                   CAT(P)    '.  '         SCMSG1
     C*
     C*
     C                   MOVEL     *BLANKS       SCMSG2
      *
     C                   CAT(P)    MSEN:1        SCMSG2
     C                   MOVEL     'AUTO LB '    TMP8              8
     C                   CAT(P)    TMP8:1        SCMSG2
      *
     C*
     C*** PROCESS THE REQUESTS ***
     C                   MOVEL     SCMSG1        ##MSG1           64
     C*
     C*
     C                   MOVEL     DLIMGQ        ##QID            10
     C*** BYPASS WRITING IF THIS RECORD ALREADY EXISTS IN THE IE FILE
     C*** ????*  IEKY1     SETLLPIE01                    67
     C*** ????*  *IN67     CABEQ*ON       BYPWRI
     C*
     C*
     C                   MOVEL     DLIMGQ        IEQID            10
     C*
     C                   MOVE      *BLANKS       IESTAT
     C                   TIME                    DSDTTM
     C     10000.01      MULT      DSDATE        IEIDT
     C                   MOVE      IEIDT         IELDT
     C                   MOVEL     Q1USER        IELUSR
     C                   MOVEL     Q1USER        IEIUSR
     C                   MOVE      DSTIME        IELTIM
     C                   MOVE      DSTIME        IEITIM
     C                   MOVEL     Q1PGM         IELPGM
     C                   MOVEL     Q1PGM         IEIPGM
     C                   MOVEL     'RL'          IEDSTS
     *** BEG ADD ********** 07/09/07 ********************************
     C                   Z-ADD     XXRCVD        IERCVD
     *** END ADD ********** 07/09/07 ********************************
     C*
     C                   WRITE     PIE0P
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
     C                   PARM                    SCMSG2           64
     C                   PARM                    SCMSG3           64
     C                   PARM                    SCMSG4           64
     C                   PARM                    SCMSG5           64
     C                   PARM                    SCMSG6           64
     C                   PARM                    SCMSG7           64
     C                   PARM                    SCMSG8           64
     C                   PARM      'N'           XXBRKM            1
     C                   PARM                    @$RTN             1
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         ##CDAT            8 0
     C                   MOVE      ##CDAT        QQCDAT
     C                   MOVE      DSTIME        Q$TIME            6
      *
     C*** FILL ALL DOC NOTE FIELDS
      *
     C                   MOVE      *BLANKS       XX75             75
     C                   MOVEL     QQCDAT        XXM               2
     C                   MOVE      QQCDAT        XXY               4
     C     2             SUBST     QQCDAT:3      XXD               2
     C     XXM           CAT(P)    '/'           XX3A              3
     C     XX3A          CAT(P)    XXD           XX5A              5
     C     XX5A          CAT(P)    '/'           XX6A              6
     C     XX6A          CAT(P)    XXY           XXMDYY           10
     C*
     C                   MOVEL     Q$TIME        XXH               2
     C                   MOVE      Q$TIME        XXS               2
     C     2             SUBST     Q$TIME:3      XXM               2
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
     C     MDOC          CAT(P)    QQDCID:1      XX75
     C                   CAT(P)    DHBR:1        XX75
     C                   CAT(P)    DTOQ:1        XX75
     C                   CAT(P)    ##QID:1       XX75
     C                   CAT(P)    '.':1         XX75
     C*
     C                   MOVEL     XX75          DN(2)
     C*
     C                   MOVEL     'AUTO LB '    TMP8              8
     C     MSEN          CAT(P)    TMP8:1        XX75
     C                   CAT(P)    '.':1         XX75
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
     C                   PARM                    QQCDAT            8              NOTE DATE
     C                   PARM      Q1PGM         QQPNAM           10              CALLING PGM
     C                   PARM      'H'           QQPRFL            1              PROTECT FLG
     C                   PARM                    Q$RTC             1              RTN CODE
     C*
     C                   ENDDO
     C*
     C                   ENDSR
