     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.              **
     H*                                                            **
     H*                                                            **
     H* PROGRAM:     SC861R                                        **
     H* DESCRIPTION: MAINTAIN SMPRP DEPENDING ON DARTS RECORDS     **
     H*              SDCMP                                         **
     H* CREATED BY:  C:12   PROJ:OP2      DATE:  05/12/93          **
     H***************************************************************
     H*  REVISIONS:                                                **
     H*                                                            **
     H*  10/17/11 C:277 Project: SY195                             **
     H*                 QDLS migration to IFS                      **
     H*                 Add two parameters with command            **
     H*                                                            **
     H*  05/14/08 C:136 SUPPORT: WA114 63486                       **
     H*                 ACCOUNT FOR ADDITIONAL RETURN CODES        **
     H*                 FROM CALL TO DLTDLO BY EXPANING ERR        **
     H*                 MSG ARRAY                                  **
     H***
     F***
     F**    02 - DELETE ISSUE - COULD DELETE RECORDS IN CORRECTLY
     F**          2/7/07 - ASSURE WE ARE ONLY PROCESSING ITEMS THAT HAVE
     F**                   COMPLETED COPY AND ARE SET FOR DELETE
     F***
     H****  02 - AOOPTICAL - SET SO "X" DOESNT CAUSE AN INDEX ERROR IN ARRAY
     H**
     H****  02 - AOOPTICAL - DATACENTER PROCESSING TO BE ADDED FOR
     F***                    OPTICAL
     H**   10/10/06 -INCLUDE DATACENTER PREFIX PROCESSING
     H*                                                               *
     H** 07/22/05 C:002 SUPP: CSI -
     H***                MOVED THE ENDIF FOR A HIT ON CM FILE DOWN
     H**                   SO NOT TO PROCESS CODE IF NO HIT OCCURS
     H*                                                            **
     H*  06/01/05 C:136 SUPPORT: OS120 43803                       **
     H*                 IF A TRANSFER RECORD EXISTS, USE THE       **
     H*                 NEW KEY INFO TO  DELETE DOCUMENTS WITH.    **
     H*                                                            **
      *  05/14/02 C:02 ADD CODE TO UPDATE THE OC RCD WITH
      *                STATUS OF PURGE JOB
      *
     H*  01/23/94 C:09 PROJECT ATB                                 **
     H*                ADD FOLDER MRGDOCS TO THE LIST OF FOLDERS   **
     H*                WHOSE DOCUMENTS SHOULD NEVER BE DELETED.    **
     H*                                                            **
     H*  05/17/00 C:128 PROJECT DR090                               **
     H*                Change process to include multiple objects  **
     H*                to be saved to tape and/or delete object.   **
     H*                Added file, SMOLL Object Location file,     **
     H*                and SMOPL multiple object purge/delete      **
     H*                request file. Added routine to read the     **
     H*                request file to drive the tape save and    **
     H*                delete of multiple objects.                 **
     H*                                                            **
     H*  07/31/00 C:128 PROJECT                                     **
     H*                Change DocID selection from the state of    **
     H*                CMIMGF.                                     **
     H*                                                            **
     H*  03/22/02 C:128 PROJECT                                     **
     H*                Added logical file SMOPL1 so that program   **
     H*                has a alternate way to update SMOPP.      **
     H*                                                            **
     H***************************************************************
     H* INDICATOR USAGE:                                           **
     H*  60                    CHAIN/DOWEQ                         **
     H*****************************************************************
     H* Value of RTNCD FOR CALL SVDLOC:
     H* Note:  Most of these messages are diagnostic and not escape.  We will
     H* return only the general escape message (return code = 13)) at this
     H* time FOR SAVDLO.
     H*  1  Requester is not enrolled in the system distribution directory
     H*  2  No documents found satisfying search specification in folder
     H*  3  Another owner is specified in the parameter "Owner Profile"
     H*  4  The doc or folder is in use at the time the command is run
     H*  5  The requester does not have the correct authority to doc/folder
     H*  6  The doc library object (document or folder) does not exist
     H*  7  The document is checked out
     H*  8  The tape device was not found
     H*  9  The folder name list is not valid.
     H* 10  Owner profile not found.
     H* 11  User not authorized to tape device
     H* 12  Cannot assign tape device.
     H* 13  Error occurred during processing of SAVDLO command
     H* 14  No document library objects saved.
     H*
     H* Note:  Most of these messages are diagnostic and not escape.  We will
     H* return only the general escape message (return code = 10 ) at this
     H* time FOR DLTDLO.
     H* Value of RTNCD FOR CALL DLDLOC:
     H* 1 Requester is not enrolled in the system distribution directory
     H* 2 No documents found satisfying search specification in folder
     H* 3 Another owner is specified in the parameter "Owner Profile"
     H* 4 The doc or folder is in use at the time the command is run
     H* 5 The requester does not have the correct authority to doc/folder
     H* 6 The doc library object (document or folder) does not exist
     H* 7 The document is checked out
     H* 8 Folder name is not correct
     H* 9 Folder does not exist
     H*10 Error occurred during processing of DLTDLO command
     H***************************************************************
     FSDCMLE    UF   E           K DISK
     FSMPRL1    UF   E           K DISK
     FSMDLL     UF   E           K DISK
     FSMDCP     IF   E             DISK
      *********** ADD 05/17/00 ***************************************
     FSMOLL     UF   E           K DISK
     FSMOPL     UF   E           K DISK
     F                                     RENAME(POP01:POP02)
     FSMOPL1    IF   E           K DISK
      ********END ADD 05/17/00 ***************************************
     F*** BEG ADD ***** 06/01/05 **************************************
     FSXDTL     IF   E           K DISK
     F*** END ADD ***** 06/01/05 **************************************

     D NM              S             10    DIM(1) CTDATA PERRCD(1)              DOCUMENT ID
     D DLM             S             80    DIM(24) CTDATA PERRCD(1)             ERROR MESSAGE
     D BAD             S             80    DIM(12) CTDATA PERRCD(1)             FOLDER NAMES
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C     *LIKE         DEFINE    CMCDAT        XXRDAT
     C     *LIKE         DEFINE    CMCDAT        X1RDAT
     C     *LIKE         DEFINE    PRKEY         XXKEY
     C     *LIKE         DEFINE    PRSEQ#        XXSEQ#
      *
     C*
     C     DLKY          KLIST
     C                   KFLD                    PRKEY
     C                   KFLD                    PRSEQ#
      *
     C     CMKY          KLIST
     C                   KFLD                    PRKEY
     C                   KFLD                    PRSEQ#
      *
     C     PRKY          KLIST
     C                   KFLD                    PRKEY
     C                   KFLD                    PRSEQ#
      *
     C     PRKY1         KLIST
     C                   KFLD                    XXKEY
     C                   KFLD                    XXSEQ#
      *********** ADD 05/17/00 ***************************************
      *
     C     OLKY          KLIST
     C                   KFLD                    OPKEY
     C                   KFLD                    OPSEQ#
     C                   KFLD                    OPSYSN
     C                   KFLD                    OPOFLR
     C                   KFLD                    OPOID
      *
      ********END ADD 05/17/00 ***************************************
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQTAPF            1
     C                   PARM                    QQTDEV           10
     C                   PARM                    QQVOLI           10
     C                   PARM                    QQJOB#            6
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
     C*
      **** ADD 05/14/02 **********************
      * CALL SCOCUPR TO MARK DELET   STARTED
      *
     C                   CALL      'SCOCUPR'
     C                   PARM      'D'           XXOP              1
     C                   PARM      'B'           XXBA              1
     C                   PARM      *BLANK        XXRTN             1
     C                   PARM      *ZEROS        XXFDT             8
     C                   PARM      *ZEROS        XXTDT             8
     C     XXRTN         CABNE     *BLANK        TERM
      *
      **** END ADD 05/14/02 ******************
     C*** BEGIN ADD *** 10/10/06 **************************************
     C                   CALL      'SA030R'
     C                   PARM      *BLANKS       ##PRFX            3
     C                   PARM      *BLANKS       ##PFLR           80
     C*** END   ADD *** 10/10/06 **************************************
     C*   READ REMARK DETAIL RECORD
     C*
      *  CLEAR ARRAYS
      *
     C                   READ(N)   PPR01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     1             CHAIN     PDC01                              93
     C* * * IF NOT DELETE PROCESS END PROGRAM
     C     *IN93         IFEQ      '0'
     C     DCDPFL        CABNE     'Y'           TERM
     C                   END
     C*
     C*
     C                   TIME                    DSDTTM
     C*
     C     PRDELF        IFEQ      'D'
     C*** ADD 2/7/07 *******************************************
     C     PRSTAT        ANDEQ     'C'
     C*** END ADD 2/7/07 ***************************************
     C     PRTSFL        ANDNE     'S'                                          "S"=SAVED
      * IF PRASTT IS 'N', THEN THIS IS A TEMPORARY DOCUMENT AND HAS NOTHING TO
      * DO WITH DARTS.
     C*
     C     QQTAPF        IFEQ      'Y'
     C     PRASTT        ANDNE     'N'
     C*
      *** BEG ADD ***** 06/01/05 *****************************************
     C     PRKY          CHAIN     PDT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  DTTOKY        XXKEY
     C                   Z-ADD     DTTOS#        XXSEQ#
     C                   ELSE
     C                   MOVEL(P)  PRKEY         XXKEY
     C                   Z-ADD     PRSEQ#        XXSEQ#
     C                   ENDIF
      *** END ADD ***** 06/01/05 *****************************************
     C*
     C                   MOVE      *BLANKS       QQDCID
     C                   MOVE      *BLANKS       QQFOLD
      *** BEG CHG ***** 06/01/05 *****************************************
     C*********  CMKY      CHAINPCM01                69
     C     PRKY1         CHAIN     PCM01                              69
      *** END CHG ***** 06/01/05 *****************************************
     C     *IN69         IFEQ      '0'
     C                   MOVEL     CMFOLD        QQFOLD
      *********** DLT 07/31/00 ***************************************
     C******     CMIMGF    IFNE ' '
     C******     CMIMGF    ANDNE'N'
     C******               MOVELCMIDOC    QQDCID
     C******               ELSE
     C******               MOVELCMDCID    QQDCID
     C******               ENDIF
      ********END DLT 07/31/00 ***************************************
      *********** ADD 07/31/00 ***************************************
     C     CMIMGF        IFEQ      'Y'
     C                   MOVEL     CMIDOC        QQDCID
     C                   ELSE
     C                   MOVEL     CMDCID        QQDCID
     C                   ENDIF
      ********END DLT 07/31/00 ***************************************
     C                   ENDIF
      *  SAVE DLO
     C*
     C                   MOVE      *BLANK        BADFLG            1
     C                   EXSR      BADFLD
     C     BADFLG        IFEQ      *BLANK
     C                   CALL      'SVDLOC'
     C                   PARM                    QQVOLI           10
     C                   PARM                    QQTDEV           10
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           80
     C                   PARM      '  '          QQRTC             2            NOTE: 2A RETURN CODE
     C*** Begin Add *** 10/17/11 **************************************
     C                   PARM      CMUNCN        QQUNCN          255
     C                   PARM      CMIFSP        QQIFSP            1
     C*** End   Add *** 10/17/11 **************************************
      *
     C     QQRTC         IFEQ      '  '
      *
      *** BEG CHG ***** 06/01/05 *****************************************
     C*********  DLKY      CHAINPDL01                60
     C     PRKY1         CHAIN     PDL01                              60
      *** END CHG ***** 06/01/05 *****************************************
     C     *IN60         IFEQ      '0'
     C                   MOVE      'Y'           DLTAPF                         TAPE
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DLDTWT
     C*
     C                   MOVE      DSTIME        DLTMWT                         TIME WRITTEN
     C                   MOVEL     Q1USER        DLUSWT                         USER
     C                   MOVE      QQVOLI        DLTVOL                         VOLUME
     C                   MOVE      QQJOB#        DLBUL#                         BATCH #
     C                   MOVEL     QQDCID        DLTDOC                         TAPE DOC ID
      *
     C                   UPDATE    PDL01
     C                   ENDIF
      *
      * MARK THE DOC AS SAVED TO TAPE
     C     PRKY          CHAIN     PPR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      'S'           PRTSFL                         TAPE STATUS FLAG
     C                   MOVE      *BLANKS       PRERDS                         ERROR DESCRIPTION
     C                   UPDATE    PPR01
     C                   ENDIF
      *
      *
      * ERROR WITH SAVDLO COMMAND
     C                   ELSE
     C*
     C*** IF RTCD IS ' 0' THEN WE MUST END DUE TO LACK OF SEQ#'S
     C*
     C     QQRTC         IFNE      ' 0'
     C     PRKY          CHAIN     PPR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      'E'           PRTSFL                         TAPE STATUS FLAG
     C                   Z-ADD     0             X
     C                   MOVE      QQRTC         X                 5 0
     C                   MOVE      *BLANKS       PRERDS                         ERROR DESCRIPTION
     C*** ADD 1/4/07 **************************************
     C     X             IFGT      0
     C     X             ANDLE     24
     C*** END ADD 1/4/07 ***********************************
     C                   MOVEL     DLM(X)        PRERDS
     C*** ADD 1/4/07 **************************************
     C                   ELSE
     C                   MOVEL     'GEN ERR '    PRERDS
     C                   ENDIF
     C*** END ADD 1/4/07 ***********************************
     C                   UPDATE    PPR01
     C                   ENDIF
     C*
     C                   ELSE
     C                   GOTO      TERM
     C                   ENDIF                                                  RTC <> 0
      *
     C                   ENDIF                                                  ENDIF QQRTC
      *
      *
     C                   ENDIF                                                  ENDIF BADFLG
      *
     C                   ENDIF                                                   TAPF = Y
      *
     C                   EXSR      DELDOC
      *
     C                   ENDIF                                                  ENDIF PRDELF
      *
     C*** READ NEXT RECORD
     C                   READ(N)   PPR01                                  60
     C                   ENDDO                                                  ENDDO *IN60
      * * *
      **** ADD 05/14/02 **********************
      * CALL SCOCUPR TO MARK COPY AS COMPLETED
      *
     C                   CALL      'SCOCUPR'
     C                   PARM      'D'           XXOP              1
     C                   PARM      'A'           XXBA              1
     C                   PARM      *BLANKS       XXRTN             1
     C                   PARM      *ZEROS        XXFDT             8
     C                   PARM      *ZEROS        XXTDT             8
      *
      **** END ADD 05/14/02 ******************
      *
      *
     C     SKIP          TAG
     C*
     C*** END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *===============================================================
     C     DELDOC        BEGSR
     C*
     C*
     C                   TIME                    DSDTTM
      *
     C     PRDELF        IFEQ      'D'
     C*** ADD 2/7/07 *******************************************
     C     PRSTAT        ANDEQ     'C'
     C*** END ADD 2/7/07 ***************************************
     C     PRTSFL        ANDNE     'E'
      *
      *** BEG ADD ***** 06/01/05 *****************************************
     C     PRKY          CHAIN     PDT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  DTTOKY        XXKEY
     C                   Z-ADD     DTTOS#        XXSEQ#
     C                   ELSE
     C                   MOVEL(P)  PRKEY         XXKEY
     C                   Z-ADD     PRSEQ#        XXSEQ#
     C                   ENDIF
      *** END ADD ***** 06/01/05 *****************************************
      *********** ADD 05/17/00 ***************************************
      *
      *** BEG CHG ***** 06/01/05 *****************************************
     C********** CMKY      CHAINPCM01               N73
     C     PRKY1         CHAIN(N)  PCM01                              73
      *** END CHG ***** 06/01/05 *****************************************
     C     CMDCID        IFEQ      *BLANKS
      *
      * This remark record does not have a remark or image linked to
      * it -but- it may have multiple objects attached.(CMQPTY='Y')
      *
     C     CMQPTY        IFEQ      'Y'
     C                   EXSR      MULTI
     C                   ENDIF
      *
     C                   ELSE
      * This a regular remark record - process it.
      ********END ADD 05/17/00 ***************************************
      *  DELETE DLO
     C                   MOVE      *BLANKS       QQFOLD
     C                   MOVE      *BLANKS       QQDCID
     C     CMKY          CHAIN     PCM01                              69
     C     *IN69         IFEQ      '0'
     C                   MOVEL     CMFOLD        QQFOLD
      *********** DLT 07/31/00 ***************************************
     C******     CMIMGF    IFNE ' '
     C******     CMIMGF    ANDNE'N'
     C******               MOVELCMIDOC    QQDCID
     C******               ELSE
     C******               MOVELCMDCID    QQDCID
     C******               ENDIF
      ********END DLT 07/31/00 ***************************************
      *********** ADD 07/31/00 ***************************************
     C     CMIMGF        IFEQ      'Y'
     C                   MOVEL     CMIDOC        QQDCID
     C                   ELSE
     C                   MOVEL     CMDCID        QQDCID
     C                   ENDIF
      ********END DLT 07/31/00 ***************************************
     C*** 7/22/05 *** MOVED NEXT LINE DOWN (END FOR THE CM HIT) ******
     C*** MOVED ****       ENDIF
     C*
     C                   MOVE      *BLANK        BADFLG            1
     C                   EXSR      BADFLD
     C*
     C     BADFLG        IFEQ      *BLANK
     C*
      *
     C*** BEGIN ADD *** 10/10/06 **************************************
     C     ##PFLR        IFNE      *BLANKS
     C                   MOVEL(P)  ##PFLR        @@WK3            80
     C                   CAT(P)    '/':0         @@WK3
     C                   CAT(P)    QQFOLD:0      @@WK3
     C                   MOVEL     @@WK3         QQFOLD
     C                   ENDIF
     C*** END   ADD *** 10/10/06 **************************************
     C                   CALL      'DLDLOC'
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           80
     C                   PARM      '  '          QQRTC             2            NOTE: 2A RETURN CODE
     C*** Begin Add *** 10/17/11 **************************************
     C                   PARM      CMUNCN        QQUNCN          255
     C                   PARM      CMIFSP        QQIFSP            1
     C*** End   Add *** 10/17/11 **************************************
      *
     C     QQRTC         IFEQ      '  '
     C     *IN69         IFEQ      '0'
     C     CMFL05        ANDEQ     'O'
     C******DEL 10/18/98** MOVE *BLANKS   CMDCID           DOC ID
     C  N60
*DEL*CAN 60              MOVEL     NM(1)         CMDCID                         DOC ID
      *
     C                   UPDATE    PCM01
      *
     C                   ENDIF
      *
     C     PRKY          CHAIN     PPR01                              60
     C                   MOVE      'C'           PRDELF                         COMPLETE
     C                   MOVE      *BLANKS       PRERDS                         ERROR DESCRIPTION
      *
     C     *IN60         IFEQ      '0'
     C                   UPDATE    PPR01
     C                   ENDIF
      *
     C                   ELSE
      *
      * ERROR WITH DLTDLO COMMAND
      *
     C     PRKY          CHAIN     PPR01                              60
      *
     C                   Z-ADD     0             X
     C                   MOVE      QQRTC         X                 5 0
      *** BEG ADD ***** 05/14/08 *****************************************
     C     X             IFGT      10
     C                   Z-ADD     10            X
     C                   ENDIF
      *** END ADD ***** 05/14/08 *****************************************
     C                   ADD       14            X
     C                   MOVE      *BLANKS       PRERDS                         ERROR DESCRIPTION
     C                   MOVEL     DLM(X)        PRERDS
      *
     C     *IN60         IFEQ      '0'
     C                   UPDATE    PPR01
     C                   ENDIF
      *
      *
     C                   ENDIF                                                  ENDIF QQRTCD
     C*
      *
     C                   ENDIF                                                  ENDIF BADFLG
      *
     C*** MOVED NEXT LINE FROM ABOVE ** 7/22/05 ** END FOR THE CM CHAIN ***
>>   C                   ENDIF                                                  ENDIF CM HIT
      *
      *********** ADD 05/17/00 ***************************************
      *
      * This record may have multiple objects attached.(CMQPTY='Y')
      *
     C     CMQPTY        IFEQ      'Y'
     C                   EXSR      MULTI
     C                   ENDIF
      *
     C                   ENDIF                                                  ENDIF CMDCID
      ********END ADD 05/17/00 ***************************************
      *
     C                   ENDIF                                                  ENDIF PRDELF
     C*
     C                   ENDSR
     C*
      *===============================================================
     C     BADFLD        BEGSR
     C*
     C*
     C                   Z-ADD     1             X                 5 0
     C     QQFOLD        LOOKUP    BAD(X)                                 60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           BADFLG
     C                   END
     C*
     C                   ENDSR
      *********** ADD 05/17/00 ***************************************
      *====================================================================*
      * * *   MULTI SUBROUTINE - Process Multiple objects              V L *
      *====================================================================*
      *
     C     MULTI         BEGSR
      *
      * Read each record with a key that is equal to the remark key.
      *
      *** BEG CHG ***** 06/01/05 *****************************************
     C********** DLKY      CHAINPOP01               N70
     C     PRKY1         CHAIN(N)  POP01                              70
      *** END CHG ***** 06/01/05 *****************************************
     C     *IN70         DOWEQ     *OFF
      *
     C*
     C     OPDELF        IFEQ      'D'
     C*** ADD 2/7/07 *******************************************
     C     PRSTAT        ANDEQ     'C'
     C*** END ADD 2/7/07 ***************************************
     C     OPTSFL        ANDNE     'S'                                          "S"=SAVED
      *
      * If PRASTT is 'N', then it is a temporary document and has
      * nothing to do with DARTS.
      *
     C     QQTAPF        IFEQ      'Y'
     C     OPASTT        ANDNE     'N'
     C*
     C                   MOVE      *BLANKS       QQDCID
     C                   MOVE      *BLANKS       QQFOLD
     C                   MOVEL     OPOFLR        QQFOLD
     C                   MOVEL     OPOID         QQDCID
      *  Save to tape
     C*
     C                   MOVE      *BLANK        BADFLG            1
     C                   EXSR      BADFLD
     C     BADFLG        IFEQ      *BLANK
     C                   CALL      'SVDLOC'
     C                   PARM                    QQVOLI           10
     C                   PARM                    QQTDEV           10
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           80
     C                   PARM      '  '          QQRTC             2            NOTE: 2A RETURN CODE
     C*** Begin Add *** 10/17/11 **************************************
     C                   PARM      *BLANKS       QQUNCP          255
     C                   PARM      *BLANKS       QQUNCF            1
     C*** End   Add *** 10/17/11 **************************************
      *
     C     QQRTC         IFEQ      '  '
      *
     C     OLKY          CHAIN     POL01                              70
     C     *IN70         IFEQ      '0'
     C                   MOVE      'Y'           OLTAPF                         TAPE
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OLDTWT
     C*
     C                   MOVE      DSTIME        OLTMWT                         TIME WRITTEN
     C                   MOVEL     Q1USER        OLUSWT                         USER
     C                   MOVE      QQVOLI        OLTVOL                         VOLUME
     C                   MOVE      QQJOB#        OLBUL#                         BATCH #
     C                   MOVEL     QQDCID        OLTDOC                         TAPE DOC ID
      *
     C                   UPDATE    POL01
     C                   ENDIF
      *
      * Mark the Doc as saved to tape
      *
      *** Begin Change 3/22/02 ***************************************
     C******     OLKY      CHAINPOP01                71
     C     OLKY          CHAIN     POP02                              71
      *** End   Change 3/22/02 ***************************************
     C     *IN71         IFEQ      *OFF
     C                   MOVE      'S'           OPTSFL                         TAPE STATUS FLAG
     C                   MOVE      *BLANKS       OPERDS                         ERROR DESCRIPTION
      *** Begin Change 3/22/02 ***************************************
     C******               UPDATPOP01
     C                   UPDATE    POP02
      *** End   Change 3/22/02 ***************************************
     C                   ENDIF
      *
      * Error with save to tape
      *
     C                   ELSE
     C*
     C*** If RTCD is ' 0' then we must end due to lack of seq#'s
     C*
     C     QQRTC         IFNE      ' 0'
      *** Begin Change 3/22/02 ***************************************
     C******     OLKY      CHAINPOP01                72
     C     OLKY          CHAIN     POP02                              72
      *** End   Change 3/22/02 ***************************************
     C     *IN72         IFEQ      *OFF
     C                   MOVE      'E'           OPTSFL                         TAPE STATUS FLAG
     C                   Z-ADD     *ZERO         X
     C                   MOVE      QQRTC         X                 5 0
     C                   MOVE      *BLANKS       OPERDS                         ERROR DESCRIPTION
     C                   MOVEL     DLM(X)        OPERDS
      *** Begin Change 3/22/02 ***************************************
     C******               UPDATPOP01
     C                   UPDATE    POP02
      *** End   Change 3/22/02 ***************************************
     C                   ENDIF
     C*
     C                   ELSE
     C                   GOTO      TERM
     C                   ENDIF                                                  RTC <> 0
      *
     C                   ENDIF                                                  ENDIF QQRTC
      *
      *
     C                   ENDIF                                                  ENDIF BADFLG
      *
     C                   ENDIF                                                  ENDIF TAPF=Y
      *
      * Multiple object delete process
      *
     C                   TIME                    DSDTTM
     C*
     C     OPDELF        IFEQ      'D'
     C*** ADD 2/7/07 *******************************************
     C     PRSTAT        ANDEQ     'C'
     C*** END ADD 2/7/07 ***************************************
     C     OPTSFL        ANDNE     'E'
      *
      *  Delete object
      *
     C                   MOVE      *BLANKS       QQFOLD
     C                   MOVE      *BLANKS       QQDCID
     C                   MOVEL     OPOFLR        QQFOLD
     C                   MOVEL     OPOID         QQDCID
      *
     C                   MOVE      *BLANK        BADFLG            1
     C                   EXSR      BADFLD
     C*
     C     BADFLG        IFEQ      *BLANK
      *
     C*** BEGIN ADD *** 10/10/06 **************************************
     C     ##PFLR        IFNE      *BLANKS
     C                   MOVEL(P)  ##PFLR        @@WK3            80
     C                   CAT(P)    '/':0         @@WK3
     C                   CAT(P)    QQFOLD:0      @@WK3
     C                   MOVEL     @@WK3         QQFOLD
     C                   ENDIF
     C*** END   ADD *** 10/10/06 **************************************
     C                   CALL      'DLDLOC'
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           80
     C                   PARM      '  '          QQRTC             2            NOTE: 2A RETURN CODE
     C*** Begin Add *** 10/17/11 **************************************
     C                   PARM      *BLANKS       QQUNCP          255
     C                   PARM      *BLANKS       QQUNCF            1
     C*** End   Add *** 10/17/11 **************************************
      *
      *
     C     QQRTC         IFEQ      '  '
      *
      *** Begin Change 3/22/02 ***************************************
     C******     OLKY      CHAINPOP01                74
     C     OLKY          CHAIN     POP02                              74
      *** End   Change 3/22/02 ***************************************
     C                   MOVE      'C'           OPDELF                         COMPLETE
     C                   MOVE      *BLANKS       OPERDS                         ERROR DESCRIPTION
      *
     C     *IN74         IFEQ      *OFF
      *** Begin Change 3/22/02 ***************************************
     C******               UPDATPOP01
     C                   UPDATE    POP02
      *** End   Change 3/22/02 ***************************************
     C                   ENDIF
      *
     C                   ELSE
      *
      * Error with delete command
      *
      *** Begin Change 3/22/02 ***************************************
     C******     OLKY      CHAINPOP01                75
     C     OLKY          CHAIN     POP02                              75
      *** End   Change 3/22/02 ***************************************
      *
     C                   Z-ADD     *ZERO         X
     C                   MOVE      QQRTC         X                 5 0
      *** BEG ADD ***** 05/14/08 *****************************************
     C     X             IFGT      10
     C                   Z-ADD     10            X
     C                   ENDIF
      *** END ADD ***** 05/14/08 *****************************************
     C                   ADD       14            X
     C                   MOVE      *BLANKS       OPERDS                         ERROR DESCRIPTION
     C                   MOVEL     DLM(X)        OPERDS
      *
     C     *IN75         IFEQ      *OFF
      *** Begin Change 3/22/02 ***************************************
     C******               UPDATPOP01
     C                   UPDATE    POP02
      *** End   Change 3/22/02 ***************************************
     C                   ENDIF
      *
      *
     C                   ENDIF                                                  ENDIF QQRTCD
      *
     C                   ENDIF                                                  ENDIF BADFLG
      *
     C                   ENDIF                                                  END PODELF 2
      *
     C                   ENDIF                                                  END PODELF 1
      *
     C     DLKY          READE     POP01                                  70
     C                   ENDDO
      *
     C                   ENDSR
      *====================================================================*
      ********END ADD 05/17/00 ***************************************
**
*OPTICAL*
** DLM
SAVDLO - Requester is not enrolled in the system distribution director
SAVDLO - No documents found satisfying search specification in folder
SAVDLO - Another owner is specified in the parameter "Owner Profile"
SAVDLO - The doc or folder is in use at the time the command is run
SAVDLO - The requester does not have the correct authority to doc/fold
SAVDLO - The doc library object (document or folder) does not exist
SAVDLO - The document is checked out
SAVDLO - The tape device was not found
SAVDLO _ The folder name list is not valid.
SAVDLO - Owner profile not found.
SAVDLO - User not authorized to tape device
SAVDLO - Cannot assign tape device.
SAVDLO - Error occurred during processing of SAVDLO command
SAVDLO - No document library objects saved.
DLTDLO - Requester is not enrolled in the system distribution director
DLTDLO - No documents found satisfying search specification in folder
DLTDLO - Another owner is specified in the parameter "Owner Profile"
DLTDLO - The doc or folder is in use at the time the command is run
DLTDLO - The requester does not have the correct authority to doc/fold
DLTDLO - The doc library object (document or folder) does not exist
DLTDLO - The document is checked out
DLTDLO - Folder name is not correct
DLTDLO - Folder does not exist
DLTDLO - Error occurred during processing of DLTDLO command
**
CLAIMDL
PLSRVDL
PCCOLECT
AUDIT
LOSSCTL
PREMCOL
MARKETDL
QUOTEDL
ACCTING
CLFORMS
QURSDL
MRGDOCS
