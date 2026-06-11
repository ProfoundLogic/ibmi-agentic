     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SC856R - OPTICAL PURGE/DELETE CPY TO OPTICAL
      *
      * * *
      * * *   IN THE BATCH, UPDATE THE
      * * *   SMPRP RECORDS WITH A "C" STATUS(COMPLETED).
      * * *
      * * * READ IN SMPRL , ALL PRSTAT = "P" (PURGE REQUESTED)
      * * *      FOR EACH, READ, CRT A FILE ON OPTICAL, AND COPY THE
      * * *      DLO TO IT.  UPDATE OF THE PRP RECORD MUST BE DONE TOO
      * * *      OPTICAL DATA IS NOT WRITTEN UNTIL CLOSE OF FILE OCCURS
      * * *      VERIFY HOW THIS WILL AFFECT DASD SPACE.
      * PURGE/DELETE FILE
      ***************************************************************
      *  REVISIONS:
      *
      *  06/01/05 C:136 SUPPORT: OS120 43803
      *                 ADD LOGIC TO WRITE AN ERROR MSG IF A PURGE
      *                 RECORD IS CREATED AND THE CLAIM SUBSEQUENTLY
      *                 TRANSFERS.
      *
     H**  C:02    120IAD   -  MODIFY TO GET THE SMOCP FILE TO RETRIEVE
     H**                  01/26/04    THE OPTICAL LIBRARY NAME
     H**                  ALSO, MODIFY NEWVL SUB TO CHECK TO SEE IF
     H**                  WE NEED TO SWITCH TO THE NEXT OPTICAL LIB
     H**                  (CURRNT OPT LIB IS FULL).
      *
      *  02/07/03 C:128 Project IM081 Seq# 35096
      *                 Program would not update Optical control
      *                 correctly if multiple objects were attached
      *                 to a Darts record. Moved the chain and
      *                 update to the individual record process.
      *
      *  07/31/02 C:128 Project OS115 Seq# 33599
      *                 If client only uses DARTS to attach multiple
      *                 objects to a remark with no document ID
      *                 the program would not update the status
      *                 of the Optical copy correctly.            **
      *                                                            **
      *  05/14/02   - ADDED CODE TO CALL SCOCUPR TO UPDATE THE
      *               OC FILE WITH PROCESS STATUS
     H*  05/05/94 C:09 PROJECT ATB                                 **
     H*                IN READING RECS SELECTED FOR PURGE (SMPRP), **
     H*                IF ASSOCIATED DARTS COMMENT RECORD HAS BEEN **
     H*                INACTIVATED OR DELETED, FLAG RECORD FOR     **
     H*                ERROR AND CONTINUE TO NEXT RECORD INSTEAD   **
     H*                OF ENDING THE PROCESS.                      **
     H*** 12/16/97 *** ADD IMAGE SYSTEM CALL FOR UPDATE OF IMAGE   **
     H*** RECORDS!!!!
     H*                                                            **
     H*  05/17/00 C:128 PROJECT DR090                              **
     H*                Change process to include multiple objects  **
     H*                to be coppied to Optical. Added SMMDL1      **
     H*                Multi-Object file, SMOLP Object Location    **
     H*                file, and SMOPL multiple object purge/delete**
     H*                request file.And routine to read, copy, and **
     H*                write to the object location and request    **
     H*                files.                                      **
     H***************************************************************
     FSMPRL     UF   E           K DISK
     FSDCMLE    UF   E           K DISK
     FSMDLL     UF A E           K DISK
     FSMOCL     UF   E           K DISK
      *********** ADD 05/17/00 ***************************************
     FSMOLL     UF A E           K DISK
     FSMMDL1    IF   E           K DISK
     FSMOPL     UF A E           K DISK
      ********END ADD 05/17/00 ***************************************
     F**** ADD 01/26/04 ***********************************************
     FSMOCP     IF   E             DISK
     F                                     RENAME(POC01:POC02)
     F*** NEXT OPTICAL LIBRARY FILE
     FSMNOL     UF   E           K DISK
     F**** END ADD 01/26/04 *******************************************
     F*** BEG ADD ***** 06/01/05 **************************************
     FSXDTL     IF   E           K DISK
     F*** END ADD ***** 06/01/05 **************************************
     F*

     D*
      *** BEG CHG ***** 06/01/05 *************************************
     D*****************   ERM     1   5 80
     D ERM             S             80    DIM(6) CTDATA PERRCD(1)
      *** END CHG ***** 06/01/05 *************************************
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  OCVOLI                 1      6
     D  ##VPRE                 1      2
     D  ##VOL#                 3      6  0
     D*
     D*
     D                 DS
     D  OCOFLD                 1      8
     D  ##FPRE                 1      4
     D  ##FLD#                 5      8  0
     D*
     D*
     D                 DS
     D  OCDCID                 1     12
     D  ##DLS                  1      3
     D  ##DC#                  4      8  0
     D  ##PRID                 9      9
     D  ##SD#                 10     12  0
     D*
     D                 DS
     D  OCOSBD                 1     12
     D  ##SUBD                 1      8
     D  ##PRI2                 9      9
     D  ##CSBD                10     12  0
     D*
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
      * =================================== *
      * * *  KEY LISTS                      *
      * =================================== *

     C*
     C*** PURGE/DELETE TRANSACTION FILE
      *
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
     C     PRKY          KLIST
     C                   KFLD                    XXSTAT            1
      *
     C*
     C*** DARTS COMMENT RECORD
      *
     C     CMKY          KLIST
     C                   KFLD                    PRKEY            40
     C                   KFLD                    PRSEQ#            5 0
      *
     C*
     C*** DOCUMENT LOCATION FILE
      *
     C     DLKY          KLIST
     C                   KFLD                    PRKEY            40
     C                   KFLD                    PRSEQ#            5 0
     C*
      *
     C*** OPTICAL CONTROL FILE
      *
     C     OCKY          KLIST
     C                   KFLD                    QQOSYS
     C                   KFLD                    QQLIBN
      *********** ADD 05/17/00 ***************************************
      *
      *********** ADD 01/26/04 ***************************************
     C*** NEXT OPTICAL LIBRARY FILE
      *
     C     NOKY          KLIST
     C                   KFLD                    QQOSYS
     C                   KFLD                    QQLIBN
      ***** END ADD 01/26/04 *****************************************
     C*
     C*** MULTIPLE OBJECTS  FILE
      *
     C     MDKY          KLIST
     C                   KFLD                    MDKEY
     C                   KFLD                    MDSEQ#
     C                   KFLD                    MDSYSN
     C                   KFLD                    MDOFLR
     C                   KFLD                    MDOID
      ********END ADD 05/17/00 ***************************************
      *
      * =================================== *
      * * *  PARM LISTS                     *
      * =================================== *
      *** PARM LIST FOR CALL TO OPTICAL COPY PROGRAM
      *
     C     PLOPT         PLIST
     C                   PARM                    SPATH            63
     C                   PARM                    TPATH            63
     C                   PARM                    QMSGD           256
     C                   PARM                    QQRTCD            1
      *
      *** PARM LIST FOR CALL TO CREATE OPTICAL DIRECTORY
      *
     C     PLCOD         PLIST
     C                   PARM                    NPATH            63
     C                   PARM                    QMDSC           256
     C                   PARM                    QQRTCD            1
      *
     C*
      * * * PROCESS RCDS SELECTED FOR PURGE
      *
     C                   MOVE      'P'           XXSTAT                              PRGD RCDS
      *
      *
     C*
     C*** OPTICAL INFO IS CURRENTLY SPECIFIC TO A SINGLE OPTICAL LIB
     C*** WE ARE USING "G" FOR SYSTEM AND "OPT01LIB" FOR LIB NAME.
     C*** WE WOULD HAVE TO CREATE A METHOD OF SELECTING THE SYS/LIB
     C*** IF WE WERE TO USE MORE THAN ONE LIB AT A TIME.
     C*
     C*** 01/26/04 ** ADD *****************************************8
     C     1             CHAIN     SMOCP                              64
     C     *IN64         IFEQ      *OFF
     C                   MOVEL     OCOSYS        QQOSYS            1
     C                   MOVEL     OCLIBN        QQLIBN           10
     C                   ENDIF
     C*** 01/26/04 ** END ADD *************************************8
     C*** DEL 1/26/04      MOVE 'G'       QQOSYS  1
     C*** DEL 1/26/04      MOVEL'OPTMLB01'QQLIBN 10
     C*
      **** ADD 05/14/02 **********************
      * CALL SCOCUPR TO MARK COPY AS STARTED
      *
     C                   CALL      'SCOCUPR'
     C                   PARM      'C'           XXOP              1
     C                   PARM      'B'           XXBA              1
     C                   PARM      *BLANKS       XXRTN             1
     C                   PARM      *ZEROS        XXFDT             8
     C                   PARM      *ZEROS        XXTDT             8
     C     XXRTN         CABNE     *BLANKS       TERM
      *
      **** END ADD 05/14/02 ******************
      *
      *
      *** READ EACH RECORD SELECTED FOR PURGE.
     C*
     C     PRKY          CHAIN     PPR01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C*
      *** Begin Change 02/07/03 **************************************
      * Moved chain to control file to individual record process
      * * * GET OPTICAL CONTROL RECORD AND LOCK IT.
      *
      ******     OCKY      CHAINPOC01                62
      *
      ******     *IN62     IFEQ '1'
      ** IF NO RECORD, ERROR***** CAN'T CONTINUE.
      *** SEND MESSAGE TO QSYSOPR AND EXIT.
      ******               GOTO TERM
      ******               ENDIF
      *
      *** ADD 05/14/2
      ******               MOVE '1'       GOTONE  1
      *** END ADD 5/14/02
      *
      ** IF PURGE PROCESS IS NOT TO RUN END THIS PROGRAM.
      ******     OCPPFL    CABNE'Y'       TERM
      *
      *** VERIFY THAT THE # OF DOCS/# OF SUBDIRS IS WITHIN RANGE.
      ***    IF NOT, CALL APPROPRIATE ROUTINE TO PERFORM CHANGES ETC.
      *
      *** IF # OF DOCS IN THE SUBDIC IS > OCM#DC, THEN CALL PGM TO
      *** CREATE A NEW SUBDIREC.
      ***
      ******     OCC#DC    CASGEOCM#DC    NEWSD            CREATE NEW SUBD
      ******               ENDCS
      *
      *** End   Change 02/07/03 **************************************
     C*
     C*** GET DARTS COMMENT RCD HERE TO FIND SOURCE PATH(SPATH)
     C*
      *** BEG ADD ***** 06/01/05 *************************************
     C     CMKY          CHAIN     PDT01                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     Q1USER        PRUSER
     C                   MOVEL     Q1PGM         PRPGM
     C                   TIME                    DSDTTM
     C                   MOVE      DSTIME        PRTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PRDT
     C                   MOVE      'E'           PRSTAT
     C                   MOVE      ERM(6)        PRERDS
     C                   UPDATE    PPR01
     C                   GOTO      NXTPR
     C                   ENDIF
      *** END ADD ***** 06/01/05 *************************************
     C*
     C     CMKY          CHAIN     PCM01                              61
     C*
     C     *IN61         IFEQ      '1'
     C                   MOVEL     Q1USER        PRUSER
     C                   MOVEL     Q1PGM         PRPGM
     C                   TIME                    DSDTTM
     C                   MOVE      DSTIME        PRTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PRDT
     C*
     C                   MOVE      'E'           PRSTAT
     C                   MOVE      ERM(4)        PRERDS
     C                   UPDATE    PPR01
     C                   GOTO      NXTPR
     C*
     C*** MUST HAVE A CMT RECORD OR CAN'T DO PURGE *** ERROR
     C*** SND MESSAGE TO QSYSOPR AND EXIT.
     C                   ELSE
     C     CMSTAT        IFEQ      'I'
     C                   MOVEL     Q1USER        PRUSER
     C                   MOVEL     Q1PGM         PRPGM
     C                   TIME                    DSDTTM
     C                   MOVE      DSTIME        PRTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PRDT
     C*
     C                   MOVE      'E'           PRSTAT
     C                   MOVE      ERM(5)        PRERDS
     C                   UPDATE    PPR01
     C                   GOTO      NXTPR
     C                   END
     C                   ENDIF
     C*
      *********** ADD 05/17/00 ***************************************
      * Since the QRYSLT that selects records to include in the
      * Purge Request file that may or maynot be eligible to process we
      * need to test for that.
      *
     C     CMDCID        IFEQ      *BLANKS
      *
      * If this remark has multiple objects attached to process them.
      * -ELSE- Do not process, get next record.
      *
     C     CMQPTY        IFEQ      'Y'
     C                   EXSR      MULTI
     C                   ELSE
     C                   GOTO      NXTPR
     C                   ENDIF
      *
      * This is a single remark, do regular process.
      *
     C                   ELSE
      ********END ADD 05/17/00 ***************************************
      *
      *** Begin Change 02/07/03 **************************************
      *
      * * * GET OPTICAL CONTROL RECORD AND LOCK IT.
      *
     C     OCKY          CHAIN     POC01                              62
      *
     C     *IN62         IFEQ      '1'
      ** IF NO RECORD, ERROR***** CAN'T CONTINUE.
      *** SEND MESSAGE TO QSYSOPR AND EXIT.
     C                   GOTO      TERM
     C                   ENDIF
      *
     C     GOTONE        IFEQ      *BLANK
     C                   MOVE      '1'           GOTONE            1
     C                   ENDIF
      *
      ** IF PURGE PROCESS IS NOT TO RUN END THIS PROGRAM.
     C     OCPPFL        CABNE     'Y'           TERM
      *
      *** VERIFY THAT THE # OF DOCS/# OF SUBDIRS IS WITHIN RANGE.
      ***    IF NOT, CALL APPROPRIATE ROUTINE TO PERFORM CHANGES ETC.
      *
      *** IF # OF DOCS IN THE SUBDIC IS > OCM#DC, THEN CALL PGM TO
      *** CREATE A NEW SUBDIREC.
      ***
     C     OCC#DC        CASGE     OCM#DC        NEWSD                          CREATE NEW SUBD
     C                   ENDCS
      *
      *** End   Change 02/07/03 **************************************
     C***  CREATE THE SRC PATH
     C     '/'           CAT(P)    CMFOLD:0      SPATH
     C                   CAT(P)    '/':0         SPATH
     C     CMIMGF        IFEQ      'Y'
     C                   CAT(P)    CMIDOC:0      SPATH
     C                   ELSE
     C                   CAT(P)    CMDCID:0      SPATH
     C                   ENDIF
     C*
     C                   ADD       1             ##DC#
     C                   MOVE      ##CSBD        ##SD#
     C                   ADD       1             OCC#DC
     C*
     C*** WILL MAKE SURE TO UPDATE THE CURRENT DOC#, ETC?????
      *
     C     RDYCPY        TAG
      *
      *** MOVE PR VALUES TO QQ(PARM LIST) VALUES FOR CALL
      *
      *
     C***  CREATE THE TARGET PATH
     C     '/'           CAT(P)    OCVOLI:0      TPATH
     C                   CAT(P)    '/':0         TPATH
     C                   CAT(P)    OCOFLD:0      TPATH
     C                   CAT(P)    '/':0         TPATH
     C                   CAT(P)    OCOSBD:0      TPATH
     C                   CAT(P)    '/':0         TPATH
     C                   CAT(P)    OCDCID:0      TPATH
     C*
     C                   MOVE      *BLANKS       QMSGD           256
     C                   MOVE      *BLANKS       QQRTCD            1
      *
     C*** CALL PROGRAM TO COPY THE DLS FILE TO OPTICAL VIA COPY STREAM FILE
     C*** API'S
     C*
     C                   CALL      'CPYTOOPT'    PLOPT
     C*
      ** ** IF UNSUCESSFUL
     C     QQRTCD        IFNE      *BLANK
     C***
     C     QQRTCD        CABEQ     '*'           TERM
     C     QQRTCD        IFEQ      '1'
     C*
     C*** IF VOL FULL, GET NEXT VOLUME, CHANGE THE OC RCD,
     C*** AND PATH INFO FOR CALL TO CPYSTRMFILE.
     C*** GOTO RDYCPY.
     C                   EXSR      NEWVL
     C                   GOTO      RDYCPY
     C                   ENDIF
     C*
     C*** HANDLE OTHER ERRORS AS NEEDED.
     C*
     C                   MOVE      'E'           PRSTAT
     C     QQRTCD        IFEQ      '2'
     C     QMSGD         IFEQ      *BLANKS
     C                   MOVE      ERM(2)        PRERDS
     C                   ELSE
     C                   MOVEL     QMSGD         PRERDS
     C                   END
     C*
     C                   END
     C*
     C     QQRTCD        IFEQ      '3'
     C     QMSGD         IFEQ      *BLANKS
     C                   MOVE      ERM(3)        PRERDS
     C                   ELSE
     C                   MOVEL     QMSGD         PRERDS
     C                   END
     C*
     C                   END
      *** Begin Change 02/07/03 **************************************
     C                   UPDATE    POC01
      *** Begin Change 02/07/03 **************************************
     C                   ELSE
     C*
      *** MARK THIS RECORD AS "C"OMPLETE (IN PRSTAT)
     C                   MOVE      'C'           PRSTAT
     C*
     C*** WRITE/UPDATE DL FILE
     C*
     C     DLKY          CHAIN     PDL01                              63
      *
     C                   MOVEL     OCDCID        DLDCID
     C                   MOVEL     QQOSYS        DLOSYS
     C                   MOVEL     OCOFLD        DLOFLD
     C                   MOVEL     OCOSBD        DLOSBD
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DLDTWR
     C*
     C                   MOVE      DSTIME        DLTMWR
     C                   MOVEL     Q1USER        DLUSWR
     C*
     C                   MOVE      'Y'           DLOPTF
     C*
     C                   MOVEL     OCVOLI        DLVOL1
     C*
     C                   MOVE      'A'           DLSTAT
     C                   MOVEL     Q1USER        DLUSER
     C                   MOVEL     Q1PGM         DLPGM
     C                   MOVE      DSTIME        DLTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DLDT
     C*
     C     *IN63         IFEQ      '0'
     C*** NO TAPE INFO WRITTEN
     C                   UPDATE    PDL01
     C                   ELSE
     C                   MOVEL     PRKEY         DLKEY
     C                   MOVE      PRSEQ#        DLSEQ#
     C*** CHANGE 1/26/04 **************************
     C                   MOVE      PRSYS         DLSYS
     C*** DEL 1/26/04 **   MOVEL'G'       DLSYS
     C                   MOVE      *ZERO         DL#BYT                         LATER USE
     C                   MOVE      *ZEROS        DLPAGS                         LATER USE
     C*** NO TAPE INFO WRITTEN
     C                   MOVEL     Q1USER        DLIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DLIDT
     C*
     C                   WRITE     PDL01
     C                   END
      *********** ADD 05/17/00 ***************************************
      *** Begin Change 02/07/03 **************************************
     C                   UPDATE    POC01
      *** Begin Change 02/07/03 **************************************
      *
      * If this remark has multiple objects attached, process them.
      *
     C     CMQPTY        IFEQ      'Y'
     C                   EXSR      MULTI
     C                   ENDIF
      *
     C                   ENDIF                                                  CMDCID End
      ********END ADD 05/17/00 ***************************************
     C*
     C*********** ADD 12/16/97 ***************************************
     C* ** CALL TO THE OPTICAL UPDATE FILES FOR IMAGING
     C*
     C                   CALL      'SIMGOPT'                            99
     C                   PARM                    DLDCID
     C                   PARM                    DLOSYS
     C                   PARM                    DLOFLD
     C                   PARM                    DLOSBD
     C                   PARM                    DLKEY
     C                   PARM                    DLSEQ#
     C                   PARM                    DLSYS
     C                   PARM                    QQLIBN
     C                   PARM                    DLVOL1
     C*
     C*
     C*********** ADD 12/16/97 ****************************************
     C*
     C****** END  ADD 12/16/97 ****************************************
     C*
     C****** Begin Chg 07/31/02 ***************************************
     C*** UPDATE THE OC FILE (LAST DCID INFO ETC)
     C*
     C                   END
     C*
      *** Begin Change 02/07/03 **************************************
      ******               UPDATPOC01
      *** Begin Change 02/07/03 **************************************
     C*
     C******************   END
     C****** End   Chg 07/31/02 ***************************************
      *
      *** OTHER PR FIELDS?????
      *
     C                   MOVEL     OCDCID        PRDCID
     C                   MOVEL     QQOSYS        PROSYS
     C                   MOVEL     OCOFLD        PROFLD
     C                   MOVEL     OCOSBD        PROSBD
     C*
     C     PRSTAT        IFEQ      'C'
     C                   MOVEL     'D'           PRDELF
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        PRUSER
     C                   MOVEL     Q1PGM         PRPGM
     C                   TIME                    DSDTTM
     C                   MOVE      DSTIME        PRTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PRDT
     C*
      *
      *** UPDATE THE PR FILE WITH "COMPLETED" INFO. ******
      *
     C                   UPDATE    PPR01
     C*
     C     PRSTAT        IFEQ      'C'
     C                   MOVE      'O'           CMFL05
     C                   UPDATE    PCM01
     C                   ENDIF
      *
     C     NXTPR         TAG
     C*
     C     PRKY          READE     PPR01                                  60
     C*
     C                   ENDDO
     C*
      **** ADD 05/14/02 **********************
      * CALL SCOCUPR TO MARK COPY AS COMPLETED
      *
     C     GOTONE        IFEQ      '1'
     C                   CALL      'SCOCUPR'
     C                   PARM      'C'           XXOP              1
     C                   PARM      'A'           XXBA              1
     C                   PARM      *BLANKS       XXRTN
     C                   PARM      *ZEROS        XXFDT             8
     C                   PARM      *ZEROS        XXTDT             8
     C                   ENDIF
      *
      **** END ADD 05/14/02 ******************
     C*
     C     TERM          TAG
     C*
      * * * GET OPTICAL CONTROL RECORD AND SHUT OFF PURGE IF NOT ALRED
      *     OFF
     C     OCKY          CHAIN     POC01                              62
      *
     C     *IN62         IFEQ      '0'
     C                   MOVE      'N'           OCPPFL
     C                   UPDATE    POC01
     C                   ENDIF
     C*
     C*
     C                   MOVE      '1'           *INLR
     C*
     C/EJECT
      *
      *====================================================================*
      * * *   NEWSD SUBROUTINE - CREATE A NEW SUBDIREC ON OPT    N E W S D *
      *====================================================================*
      *
     C     NEWSD         BEGSR
     C*
     C*** FIRST CHECK MAX # OF SUBDIRECS IS NOT EXCEEDED.
     C*** IF NOT CREATE A NEW SUBDIREC IN EXISTING FOLDER
     C*** ELSE CREATE A NEW FOLDER, AND A NEW SUBDIRECTORY.
     C*** IN ALL CASES, UPDATE THE OC RECORD, CLEARING THE LAST DOCID,
     C*** THE CURRENT SUBDIREC, AND IF APPLICABLE THE CURRENT FOLDER NAME.
     C*
     C     OCC#SD        IFGE      OCM#SD                                        (USE XX FOR #)
     C*
     C*** CHANGE FOLDER NAME IN OC FILE, CREATE NEW ON OPT VOLUME
     C                   ADD       1             ##FLD#
      *
     C***  CREATE THE NEW PATH
     C     '/'           CAT(P)    OCVOLI:0      NPATH
     C                   CAT(P)    '/':0         NPATH
     C                   CAT(P)    OCOFLD:0      NPATH
     C                   MOVE      *BLANK        QQRTCD
     C                   MOVE      *BLANK        QMDSC
      *
     C                   CALL      'CRTOPTD'     PLCOD
     C     QQRTCD        CABEQ     '*'           TERM
     C     QQRTCD        IFEQ      '1'
     C                   END
     C     QQRTCD        IFEQ      '2'
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   END
     C*
     C*
     C*** CHANGE SUBDIREC TO 001
     C                   Z-ADD     1             OCC#SD
     C                   MOVE      001           ##CSBD
      *
     C***  CREATE THE NEW PATH
     C     '/'           CAT(P)    OCVOLI:0      NPATH
     C                   CAT(P)    '/':0         NPATH
     C                   CAT(P)    OCOFLD:0      NPATH
     C                   CAT(P)    '/':0         NPATH
     C                   CAT(P)    OCOSBD:0      NPATH
     C                   MOVE      *BLANK        QQRTCD
     C                   MOVE      *BLANK        QMDSC
     C*
     C                   CALL      'CRTOPTD'     PLCOD
     C     QQRTCD        CABEQ     '*'           TERM
     C     QQRTCD        IFEQ      '1'
     C                   END
     C     QQRTCD        IFEQ      '2'
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   END
     C*
      *
     C* * * CHANGE DOC TO DLS00000.XXX
     C                   MOVE      *ZEROS        ##DC#
     C                   MOVE      ##CSBD        ##SD#
      *
      * CURRENT # OF DOCS IN SUBDIRECTORY
     C                   Z-ADD     *ZEROS        OCC#DC
     C*
     C                   ELSE
     C*** ADD 1 TO # OF SUBDIRECS AND UPDATE THE OC FILE WITH
     C*** THE NEW SUBDIREC.XXX, CUR # + 1, RESET THE DOCID, AND CUR # TO 0.
     C***
     C                   ADD       1             OCC#SD
     C                   ADD       1             ##CSBD
      *
     C***  CREATE THE NEW PATH
     C     '/'           CAT(P)    OCVOLI:0      NPATH
     C                   CAT(P)    '/':0         NPATH
     C                   CAT(P)    OCOFLD:0      NPATH
     C                   CAT(P)    '/':0         NPATH
     C                   CAT(P)    OCOSBD:0      NPATH
     C                   MOVE      *BLANK        QQRTCD
     C                   MOVE      *BLANK        QMDSC
     C*
     C                   CALL      'CRTOPTD'     PLCOD
     C*
     C     QQRTCD        CABEQ     '*'           TERM
     C     QQRTCD        IFEQ      '1'
     C                   END
     C     QQRTCD        IFEQ      '2'
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   END
     C*
      *
     C* * * CHANGE DOC TO DLS00000.XXX
     C                   MOVE      *ZEROS        ##DC#
     C                   MOVE      ##CSBD        ##SD#
      *
      * CURRENT # OF DOCS IN SUBDIRECTORY
     C                   Z-ADD     *ZEROS        OCC#DC
     C*
     C*
     C*
     C                   END
     C*
     C*****************************
     C                   UPDATE    POC01
     C     OCKY          CHAIN     POC01                              66
     C*
     C*
     C*
     C     ENEWSD        ENDSR
     C*
     C/EJECT
      *
      *====================================================================*
      * * *   NEWVL SUBROUTINE - CHANGE TO A NEW VOLUME ON OPT   N E W V L *
      *====================================================================*
      *
     C     NEWVL         BEGSR
     C*
     C*** VOLUME IS FULL. WE CAN CHANGE THE PROGRAM TO USE NEXT VOLUME.
     C*** WE MAY SEND A MESSAGE TO THE OPERATOR TO REMIND THEM OF THE NEW
     C*** NEW VOLUME REQUIRED.  IF IT ISN'T LOADED THEN IT WILL ALLOW
     C*** THE USER TO OPERATOR TO CREATE/LOAD THE VOLUME.
     C*
     C*** WE WILL NEED TO SET THE OC RECORD WITH THE NEW VOLID,
     C*** RESET THE FOLDER, SUBDIREC, ETC.(# OF DOCS ETC.).
     C*** WE WILL ALSO HAVE TO CREATE THE FOLD/SUBDIREC ETC. ON THE
     C*** PLATTER.
     C*
     C                   ADD       1             ##VOL#
     C*
     C*** CHANGE FOLDER NAME IN OC FILE, CREATE NEW ON OPT VOLUME
     C                   Z-ADD     1             ##FLD#
      *
     C***  CREATE THE NEW PATH
     C     '/'           CAT(P)    OCVOLI:0      NPATH
     C                   CAT(P)    '/':0         NPATH
     C                   CAT(P)    OCOFLD:0      NPATH
     C                   MOVE      *BLANK        QQRTCD
     C                   MOVE      *BLANK        QMDSC
     C*
     C                   CALL      'CRTOPTD'     PLCOD
     C*
     C     QQRTCD        CABEQ     '*'           TERM
     C     QQRTCD        IFEQ      '1'
     C                   END
     C     QQRTCD        IFEQ      '2'
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   END
     C*
     C*** CHANGE SUBDIREC TO 001
     C                   Z-ADD     1             OCC#SD
     C                   MOVE      001           ##CSBD
      *
     C***  CREATE THE NEW PATH
     C     '/'           CAT(P)    OCVOLI:0      NPATH
     C                   CAT(P)    '/':0         NPATH
     C                   CAT(P)    OCOFLD:0      NPATH
     C                   CAT(P)    '/':0         NPATH
     C                   CAT(P)    OCOSBD:0      NPATH
     C                   MOVE      *BLANK        QQRTCD
     C                   MOVE      *BLANK        QMDSC
     C*
     C                   CALL      'CRTOPTD'     PLCOD
     C*
     C     QQRTCD        CABEQ     '*'           TERM
     C     QQRTCD        IFEQ      '1'
     C                   END
     C     QQRTCD        IFEQ      '2'
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   END
     C*
      *
     C* * * CHANGE DOC TO DLS00000.XXX
     C                   MOVE      *ZEROS        ##DC#
     C                   MOVE      ##CSBD        ##SD#
      *
      * CURRENT # OF DOCS IN SUBDIRECTORY
     C                   Z-ADD     *ZEROS        OCC#DC
     C*
     C*****************************
     C*** ADD 1/26/04 *******************************
     C*
     C     NOKY          CHAIN     PNO01                              76
     C     *IN76         IFEQ      *OFF
     C     OCVOLI        IFEQ      NOVOLI
     C** NEW LIBRARY, MARK AS FULL, CHANGE OC VALS TO NEW LIBRARY
     C                   MOVEL     'F'           NOFULL
     C                   MOVEL     NONOSY        OCOSYS
     C                   MOVEL     NONLIB        OCLIBN
     C                   MOVEL     NONOSY        QQOSYS
     C                   MOVEL     NONLIB        QQLIBN
     C                   UPDATE    PNO01
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C*** END ADD 1/26/04 ***************************
     C                   UPDATE    POC01
     C     OCKY          CHAIN     POC01                              66
     C*
     C*
     C*
     C     ENEWVL        ENDSR
      *
      *********** ADD 05/17/00 ***************************************
      *====================================================================*
      * * *   MULTI SUBROUTINE - Process Multiple objects              V L *
      *====================================================================*
      *
     C     MULTI         BEGSR
      *
      * Read each record with a key that is equal to the remark key.
      *
     C     DLKY          CHAIN     PMD01                              70
      *
     C     *IN70         DOWEQ     *OFF
      *
      *** Begin Change 02/07/03 **************************************
      *
      * * * GET OPTICAL CONTROL RECORD AND LOCK IT.
      *
     C     OCKY          CHAIN     POC01                              62
      *
     C     *IN62         IFEQ      '1'
      ** IF NO RECORD, ERROR***** CAN'T CONTINUE.
      *** SEND MESSAGE TO QSYSOPR AND EXIT.
     C                   GOTO      TERM
     C                   ENDIF
      *
     C     GOTONE        IFEQ      *BLANK
     C                   MOVE      '1'           GOTONE            1
     C                   ENDIF
      *
      ** IF PURGE PROCESS IS NOT TO RUN END THIS PROGRAM.
     C     OCPPFL        CABNE     'Y'           TERM
      *
      *** VERIFY THAT THE # OF DOCS/# OF SUBDIRS IS WITHIN RANGE.
      ***    IF NOT, CALL APPROPRIATE ROUTINE TO PERFORM CHANGES ETC.
      *
      *** IF # OF DOCS IN THE SUBDIC IS > OCM#DC, THEN CALL PGM TO
      *** CREATE A NEW SUBDIREC.
      ***
     C     OCC#DC        CASGE     OCM#DC        NEWSD                          CREATE NEW SUBD
     C                   ENDCS
      *
      *** End   Change 02/07/03 **************************************
     C***  Create the SRC path
     C     '/'           CAT(P)    MDOFLR:0      SPATH
     C                   CAT(P)    '/':0         SPATH
     C                   CAT(P)    MDOID:0       SPATH
     C*
     C                   ADD       1             ##DC#
     C                   MOVE      ##CSBD        ##SD#
     C                   ADD       1             OCC#DC
      *
     C     RDYCP1        TAG
      *
      *** Move PR values to QQ(PARM LIST) values for call
      *
      *
     C***  Create the target path
     C     '/'           CAT(P)    OCVOLI:0      TPATH
     C                   CAT(P)    '/':0         TPATH
     C                   CAT(P)    OCOFLD:0      TPATH
     C                   CAT(P)    '/':0         TPATH
     C                   CAT(P)    OCOSBD:0      TPATH
     C                   CAT(P)    '/':0         TPATH
     C                   CAT(P)    OCDCID:0      TPATH
     C*
     C                   MOVE      *BLANKS       QMSGD           256
     C                   MOVE      *BLANKS       QQRTCD            1
      *
     C*** Call program to copy the DLS file to OPTICAL via copy stream FILE
     C*** API'S
     C*
     C                   CALL      'CPYTOOPT'    PLOPT
     C*
      ** ** If unsucessful
     C     QQRTCD        IFNE      *BLANK
      *
     C     QQRTCD        CABEQ     '*'           TERM
     C     QQRTCD        IFEQ      '1'
     C*
     C*** If vol full, get next volume, change the OC rcd,
     C*** and path info for call to CPYSTRMFILE.
     C*** goto RDYCP1.
     C                   EXSR      NEWVL
     C                   GOTO      RDYCP1
     C                   ENDIF
     C*
     C*** Handle other errors as needed.
     C*
     C                   MOVE      'E'           OPSTAT
     C     QQRTCD        IFEQ      '2'
     C     QMSGD         IFEQ      *BLANKS
     C                   MOVE      ERM(2)        OPERDS
     C                   ELSE
     C                   MOVEL     QMSGD         OPERDS
     C                   END
     C*
     C                   END
     C*
     C     QQRTCD        IFEQ      '3'
     C     QMSGD         IFEQ      *BLANKS
     C                   MOVE      ERM(3)        OPERDS
     C                   ELSE
     C                   MOVEL     QMSGD         OPERDS
     C                   END
     C*
     C                   END
     C*
     C                   ELSE
      *** Mark this record as "C"complete (in OPSTAT)
     C                   MOVE      'C'           OPSTAT
      *** Begin Add 07/31/02 ******************************************
     C                   MOVE      'C'           PRSTAT
      *** End   Add 07/31/02 ******************************************
      *
      *** Write/update OL file
      *
     C     MDKY          CHAIN     POL01                              71
      *
     C                   MOVEL     OCDCID        OLDCID
     C                   MOVEL     QQOSYS        OLOSYS
     C                   MOVEL     OCOFLD        OLOFLD
     C                   MOVEL     OCOSBD        OLOSBD
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OLDTWR
     C*
     C                   MOVE      DSTIME        OLTMWR
     C                   MOVEL     Q1USER        OLUSWR
     C*
     C                   MOVE      'Y'           OLOPTF
     C*
     C                   MOVEL     OCVOLI        OLVOL1
     C*
     C                   MOVE      'A'           OLSTAT
     C                   MOVEL     Q1USER        OLUSER
     C                   MOVEL     Q1PGM         OLPGM
     C                   MOVE      DSTIME        OLTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OLDT
     C*
     C     *IN71         IFEQ      '0'
     C*** No tape info written
     C                   UPDATE    POL01
      *
     C                   ELSE
      *
     C                   MOVEL     PRKEY         OLKEY
     C                   MOVE      PRSEQ#        OLSEQ#
     C                   MOVEL     MDSYSN        OLSYSN
     C                   MOVEL     MDOFLR        OLOFLR
     C                   MOVEL     MDOID         OLOID
     C*** CHANGE 1/26/04 *************************
     C***
     C                   MOVEL     PRSYS         OLSYS
     C*** DEL 1/26/04 **   MOVEL'G'       OLSYS
     C                   MOVE      *ZERO         OL#BYT                         LATER USE
     C                   MOVE      *ZEROS        OLPAGS                         LATER USE
     C*** No tape info written
     C                   MOVEL     Q1USER        OLIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OLIDT
     C*
     C                   WRITE     POL01
     C                   END
      *
      *** Write/update OP file
      *
     C     MDKY          CHAIN     POP01                              72
      *
     C                   MOVEL     OCDCID        OPDCID
     C                   MOVEL     QQOSYS        OPOSYS
     C                   MOVEL     OCOFLD        OPOFLD
     C                   MOVEL     OCOSBD        OPOSBD
      *
     C                   MOVEL     Q1USER        OPUSER
     C                   MOVEL     Q1PGM         OPPGM
     C                   MOVE      DSTIME        OPTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OPDT
     C*
     C     OPSTAT        IFEQ      'C'
     C                   MOVEL     'D'           OPDELF
     C                   ENDIF
     C*
     C*
     C     *IN72         IFEQ      '0'
      *
     C                   UPDATE    POP01
      *
     C                   ELSE
      *
     C                   MOVEL     PRKEY         OPKEY
     C                   MOVE      PRSEQ#        OPSEQ#
     C                   MOVEL     MDSYSN        OPSYSN
     C                   MOVEL     MDOFLR        OPOFLR
     C                   MOVEL     MDOID         OPOID
     C*** CHANGE 1/26/04 *************************
     C****DEL 1/26/04 **   MOVEL'G'       OPSYS
     C                   MOVEL     PRSYS         OPSYS
     C                   MOVE      *BLANKS       OPHCMD
     C                   MOVE      *BLANKS       OPCPRY
     C                   MOVE      *BLANKS       OPFOLD
     C                   MOVE      *BLANKS       OPSUBD
     C                   MOVE      *BLANKS       OPPNAM
     C                   MOVE      *BLANKS       OPASTT
     C                   MOVE      *ZERO         OPBUL#
     C                   MOVE      *BLANKS       OPTSFL
      *
     C                   MOVEL     Q1USER        OPIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OPIDT
      *
     C                   WRITE     POP01
     C                   ENDIF
      *
     C                   ENDIF
      *** Begin Change 02/07/03 **************************************
     C                   UPDATE    POC01
      *** Begin Change 02/07/03 **************************************
      *
     C     DLKY          READE     PMD01                                  70
     C                   ENDDO
     C                   ENDSR
      *====================================================================*
      ********END ADD 05/17/00 ***************************************
**
Optical Volume is Full(Threshold Percent has been reached).
ERROR - Path Name used is not valid.
ERROR - Document not written to Optical Library.  See Job Log for details.
ERROR - Matching DARTS comment record could not be found.
ERROR - Matching DARTS comment record has been inactivated.
ERROR - DARTS record has been transferred.
