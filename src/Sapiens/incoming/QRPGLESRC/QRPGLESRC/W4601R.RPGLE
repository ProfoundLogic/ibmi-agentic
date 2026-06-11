     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W4601R - REPORT ORDERING SUBMIT JOB
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/11/98 C:69 Support: 15495                                  *
     H*               Correct date field sizes, first digit lost      *
     H*               (Code not documented since it never worked)     *
     H*****************************************************************
     FWDUSL1    IF   E           K DISK
     FWDUSL     IF   E           K DISK
     F                                     RENAME(PUS01:PUS02)
     FWAURL     UF A E           K DISK
     FWMRDL     IF   E           K DISK
     FWMUDL     IF   E           K DISK
     FWARNL     UF A E           K DISK
     FSMCOL     IF   E           K DISK
     F*SMUPL   IF  E           K        DISK
     FLABL      O    F   80        PRINTER OFLIND(*INOA)
     F*****************************************************************

     D IN              S              1    DIM(78)
     D*****************************************************************
     D UROF            DS
     D  UROF01                 1     10
     D  UROF02                11     20
     D  UROF03                21     30
     D  UROF04                31     40
     D  UROF05                41     50
     D  UROF06                51     60
     D  UROF07                61     70
     D  UROF08                71     80
     D  UROF09                81     90
     D  UROF10                91    100
     D RDOL            DS
     D  RDOL01                 1     10
     D  RDOL02                11     20
     D  RDOL03                21     30
     D  RDOL04                31     40
     D  RDOL05                41     50
     D  RDOL06                51     60
     D  RDOL07                61     70
     D  RDOL08                71     80
     D  RDOL09                81     90
     D  RDOL10                91    100
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  CM                     1    256
     D                                     DIM(256)
     D  XXCOMP                 1    256
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I***********************************************************
     IPUS02
     I              USCO#                       U3CO#
     I              USFND                       U3FND
     I              USURNM                      U3URNM
     I              USKEY                       U3KEY
     I              USDEST                      U3DEST
     I              USUSER                      U3USER
     I              USPGM                       U3PGM
     I              USTIME                      U3TIME
     I              USDT                        U3DT
     I              USSTAT                      U3STAT
     I****************************************************************

     C*
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    XXUSER           10
     C*
     C*   KEY LIST FOR THE USER REPORT DETAIL FILE
     C*
     C     USKY1         KLIST
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR THE USER REPORT DETAIL FILE
     C*
     C     USKY2         KLIST
     C                   KFLD                    XXURNM
     C*
     C*   KEY LIST FOR THE USER DESTINATION FILE
     C*
     C     UDKEY         KLIST
     C                   KFLD                    U3URNM
     C*
     C*   KEY LIST FOR THE REPORT DEFINITION FILE
     C*
     C     RDKY1         KLIST
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR THE WORK FILE
     C*
     C     URKY1         KLIST
     C                   KFLD                    XXKEY
     C                   KFLD                    XXRNDT
     C                   KFLD                    XXSEQ#
     C*
     C*   KEY LIST FOR THE WORK FILE
     C*
     C*          URKY2     KLIST
     C*                    KFLD           XXKEY
     C*                    KFLD           XXRNDT
     C*                    KFLD           XXSEQ#
     C*                    KFLD           XXRTCD
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE COMPLETION FILE
     C*
     C     RNKEY         KLIST
     C                   KFLD                    X2RNDT
     C                   KFLD                    QQFREQ
     C*************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQRNDT            8
     C                   PARM                    QQFREQ            1
     C                   PARM                    QQRTCD            1
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
     C*************************************************************
     C* DEFINITIONS:
     C*          *LIKE     DEFN UDUSID    XXUSID
     C     *LIKE         DEFINE    USKEY         XXKEY
     C     *LIKE         DEFINE    USURNM        XXURNM
     C     *LIKE         DEFINE    USCO#         XXCO#
     C     *LIKE         DEFINE    RDPGNM        XXPGNM
     C     *LIKE         DEFINE    RDADPM        XXPRMS
     C     *LIKE         DEFINE    USURNM        SVURNM
     C     *LIKE         DEFINE    URRNDT        XXRNDT
     C     *LIKE         DEFINE    URRNDT        X2RNDT
     C     *LIKE         DEFINE    URSEQ#        XXSEQ#
     C     *LIKE         DEFINE    URRTCD        XXRTCD
     C     *LIKE         DEFINE    URRPKY        XXRPKY
     C     *LIKE         DEFINE    URRPKY        QQRPKY
     C     *LIKE         DEFINE    URPRM         XXPRM
     C     *LIKE         DEFINE    URPPGM        XXPPGM
     C     *LIKE         DEFINE    URPUSR        XXPUSR
     C     *LIKE         DEFINE    URJOBN        XXJOBN
     C     *LIKE         DEFINE    URRPGN        XXRPGN
     C     *LIKE         DEFINE    UROF01        XXOF01
     C     *LIKE         DEFINE    UROF02        XXOF02
     C     *LIKE         DEFINE    UROF03        XXOF03
     C     *LIKE         DEFINE    UROF04        XXOF04
     C     *LIKE         DEFINE    UROF05        XXOF05
     C     *LIKE         DEFINE    UROF06        XXOF06
     C     *LIKE         DEFINE    UROF07        XXOF07
     C     *LIKE         DEFINE    UROF08        XXOF08
     C     *LIKE         DEFINE    UROF09        XXOF09
     C     *LIKE         DEFINE    UROF10        XXOF10
     C     *LIKE         DEFINE    URMSG         XXMSG
     C     *LIKE         DEFINE    UDATTN        XDATTN
     C     *LIKE         DEFINE    UDCOMP        XDCOMP
     C     *LIKE         DEFINE    UDADR1        XDADR1
     C     *LIKE         DEFINE    UDADR2        XDADR2
     C     *LIKE         DEFINE    UDCTY         XDCTY
     C     *LIKE         DEFINE    UDST          XDST
     C     *LIKE         DEFINE    UDZIP1        XDZIP1
     C     *LIKE         DEFINE    UDZIP2        XDZIP2
     C*
     C                   TIME                    DSDTTM
     C*******************  MOVE QQRNDT    X2RNDT
     C                   MOVE      QQRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X2RNDT
     C                   MOVE      X2RNDT        XXRNDT
     C                   MOVE      '1'           *INOA
     C*
     C                   EXSR      INIT
     C                   EXSR      CLRWK
     C*
     C                   MOVE      '1'           XXPASS            1
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C* FOR RE-START PURPOSES, BEFORE STARTING THE READ OF THE
     C* USER-REPORT DEFINITION FILE CHECK TO SEE IF AN ENTRY
     C* EXISTS IN THE RUN DATE COMPLETION FILE. THIS WOULD BE
     C* THE REPORT TO PICK UP WITH IN THE EVENT OF A RESTART.
     C* I.E. IT IS THE LAST COMPLETED REPORT.
     C*
     C     RNKEY         CHAIN     PRN01                              60
     C     *IN60         IFEQ      '0'
     C     RNPASS        IFEQ      '2'
     C     RNURNM        SETLL     PUS02
     C                   GOTO      PASS2
     C                   ENDIF
     C                   MOVE      RNRPNM        XXKEY
     C                   Z-ADD     RNSPL#        SPL#
     C     USKY1         SETLL     PUS01
     C                   END
     C*
     C*   START READING THE USER - REPORT DETAIL FILE
     C*   THIS READ CYCLE IS BY REPORT DEFINITION KEY
     C*   ALL ACTIVE REPORT DEFINITIONS WITH ANY USER
     C*   ATTACHEMNT WILL BE PRODUCED THE SPOOL FILE
     C*   WILL BE IN OUTQ-RPTORD AND THE OFFICE DOC
     C*   WILL BE IN FOLDER-RPTDOC
     C*
     C                   READ      PUS01                                  60
     C     *IN60         DOWEQ     '0'
     C     USSTAT        IFEQ      'I'
     C     USFREQ        ORNE      QQFREQ
     C                   GOTO      NXT
     C                   END
     C*
     C*   DETERMINE IF REPORT HAS BEEN RUN. IF SO, GO TO NEXT
     C*   RECORD. REPORT DEFINITIONS SHOULD ONLY BE RAN ONCE.
     C*
     C                   MOVE      USKEY         XXKEY
     C     URKY1         CHAIN     PUR01                              60
     C     *IN60         IFEQ      '0'
     C                   GOTO      NXT
     C                   END
     C*
     C                   MOVEL     USKEY         XXKEY
     C*
     C     RDKY1         CHAIN     PRD01                              62
     C* IF USER/REPORT RECORD IS ACTIVE BUT THE REPORT DEFINITION
     C* IS INACTIVE THEN AN EXCEPTION WILL BE WRITTEN TO THE
     C* DATABASE. NO ATTEMPT WILL BE MADE TO PRODUCE THE REPORT.
     C     RDSTAT        IFEQ      'I'
     C                   MOVE      '1'           XXRTCD
     C                   MOVE      RDKEY         XXRPKY
     C                   EXSR      ERRCD
     C                   GOTO      NXT
     C                   END
     C*
     C                   MOVEL     USURNM        XXUSER
     C*
     C* BUILD THE COMMAND TO BE EXECUTED BY THE CLP-W4601C
     C*
     C                   MOVEA     'CALL'        CM
     C                   Z-ADD     6             I1                5 0
     C                   MOVEA     RDPGNM        CM(I1)
     C                   Z-ADD     17            I1
     C                   MOVEA     RDADPM        CM(I1)
     C                   MOVE      256           XXLEN
     C* SCAN FOR THE FOLLOWING RESERVED WORDS IN THE PARAMETERS LIST
     C* IF FOUND THEN THE PASSED IN PARM DATES WILL BE INSERTED OVER
     C* THE RESERVED WORDS
     C                   Z-ADD     1             X1
     C     X1            DOUEQ     0
     C     'FRDATE'      SCAN      XXCOMP:X1     X1                5 0
     C     X1            IFNE      0
     C     X1            ADD       6             X2                3 0
     C                   SUBST(P)  XXCOMP:X2     EOL             256
     C                   MOVEA     QQFRDT        CM(X1)
     C                   ADD       8             X1
     C                   MOVEA     EOL           CM(X1)
     C                   ENDIF
     C                   ENDDO
     C*
     C                   Z-ADD     1             X1
     C     X1            DOUEQ     0
     C     'TODATE'      SCAN      XXCOMP:X1     X1                5 0
     C     X1            IFNE      0
     C     X1            ADD       6             X2
     C                   SUBST(P)  XXCOMP:X2     EOL
     C                   MOVEA     QQTODT        CM(X1)
     C                   ADD       8             X1
     C                   MOVEA     EOL           CM(X1)
     C                   ENDIF
     C                   ENDDO
     C*
     C                   Z-ADD     1             X1
     C     X1            DOUEQ     0
     C     'RNDATE'      SCAN      XXCOMP:X1     X1                5 0
     C     X1            IFNE      0
     C     X1            ADD       6             X2
     C                   SUBST(P)  XXCOMP:X2     EOL
     C                   MOVEA     QQRNDT        CM(X1)
     C                   ADD       8             X1
     C                   MOVEA     EOL           CM(X1)
     C                   ENDIF
     C                   ENDDO
     C* LOAD ALL PARAMETERS
     C                   MOVEL     RDKEY         XXRPKY
     C                   MOVEL     RDRPGN        XXRPGN
     C                   MOVEL     RDOF01        XXOF01
     C     RDOF01        IFNE      *BLANKS
     C                   MOVE      *ZEROS        XXOF1#            8
     C                   MOVEL     'QSP'         XXOF1#
     C                   MOVE      SPL#          XXOF1#
     C                   ADD       1             SPL#
     C                   END
     C                   MOVEL     RDOF02        XXOF02
     C     RDOF02        IFNE      *BLANKS
     C                   MOVE      *ZEROS        XXOF2#            8
     C                   MOVEL     'QSP'         XXOF2#
     C                   MOVE      SPL#          XXOF2#
     C                   ADD       1             SPL#
     C                   END
     C                   MOVEL     RDOF03        XXOF03
     C     RDOF03        IFNE      *BLANKS
     C                   MOVE      *ZEROS        XXOF3#            8
     C                   MOVEL     'QSP'         XXOF3#
     C                   MOVE      SPL#          XXOF3#
     C                   ADD       1             SPL#
     C                   END
     C                   MOVEL     RDOF04        XXOF04
     C     RDOF04        IFNE      *BLANKS
     C                   MOVE      *ZEROS        XXOF4#            8
     C                   MOVEL     'QSP'         XXOF4#
     C                   MOVE      SPL#          XXOF4#
     C                   ADD       1             SPL#
     C                   END
     C                   MOVEL     RDOF05        XXOF05
     C     RDOF05        IFNE      *BLANKS
     C                   MOVE      *ZEROS        XXOF5#            8
     C                   MOVEL     'QSP'         XXOF5#
     C                   MOVE      SPL#          XXOF5#
     C                   ADD       1             SPL#
     C                   END
     C                   MOVEL     RDOF06        XXOF06
     C     RDOF06        IFNE      *BLANKS
     C                   MOVE      *ZEROS        XXOF6#            8
     C                   MOVEL     'QSP'         XXOF6#
     C                   MOVE      SPL#          XXOF6#
     C                   ADD       1             SPL#
     C                   END
     C                   MOVEL     RDOF07        XXOF07
     C     RDOF07        IFNE      *BLANKS
     C                   MOVE      *ZEROS        XXOF7#            8
     C                   MOVEL     'QSP'         XXOF7#
     C                   MOVE      SPL#          XXOF7#
     C                   ADD       1             SPL#
     C                   END
     C                   MOVEL     RDOF08        XXOF08
     C     RDOF08        IFNE      *BLANKS
     C                   MOVE      *ZEROS        XXOF8#            8
     C                   MOVEL     'QSP'         XXOF8#
     C                   MOVE      SPL#          XXOF8#
     C                   ADD       1             SPL#
     C                   END
     C                   MOVEL     RDOF09        XXOF09
     C     RDOF09        IFNE      *BLANKS
     C                   MOVE      *ZEROS        XXOF9#            8
     C                   MOVEL     'QSP'         XXOF9#
     C                   MOVE      SPL#          XXOF9#
     C                   ADD       1             SPL#
     C                   END
     C                   MOVEL     RDOF10        XXOF10
     C     RDOF10        IFNE      *BLANKS
     C                   MOVE      *ZEROS        XXOF0#            8
     C                   MOVEL     'QSP'         XXOF0#
     C                   MOVE      SPL#          XXOF0#
     C                   ADD       1             SPL#
     C                   END
     C* THIS PROGRAM WILL RUN THE REPORT DEFINITION, COPY THE OUTPUT
     C* TO A DATABASE.
     C                   CALL      'W4601C'
     C                   PARM                    CM
     C                   PARM      256           XXLEN            15 5
     C                   PARM                    Q1PGM
     C                   PARM                    Q1USER
     C                   PARM                    Q1JOBN
     C                   PARM                    XXRPKY
     C                   PARM                    XXRPGN
     C                   PARM                    XXOF01
     C                   PARM                    XXOF02
     C                   PARM                    XXOF03
     C                   PARM                    XXOF04
     C                   PARM                    XXOF05
     C                   PARM                    XXOF06
     C                   PARM                    XXOF07
     C                   PARM                    XXOF08
     C                   PARM                    XXOF09
     C                   PARM                    XXOF10
     C                   PARM                    XXOF1#
     C                   PARM                    XXOF2#
     C                   PARM                    XXOF3#
     C                   PARM                    XXOF4#
     C                   PARM                    XXOF5#
     C                   PARM                    XXOF6#
     C                   PARM                    XXOF7#
     C                   PARM                    XXOF8#
     C                   PARM                    XXOF9#
     C                   PARM                    XXOF0#
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQMSG             7
     C* MOVE THE RETURN CODE AND MESSAGE ID INTO VARS
     C* THEN CALL THE ERROR CODE ROUTINE WHICH WILL
     C* WRITE A RECORD FOR EACH USER/REPORT RECORD EITHER
     C* A SUCCESSFUL COMPLETION OR AN ERROR COMPLETION
     C                   MOVE      QQRTCD        XXRTCD
     C                   MOVE      QQMSG         XXMSG
     C*
     C                   EXSR      ERRCD
     C*
     C* PRIOR TO READING THE NEXT RECORD WRITE THE LAST COMPLETED
     C* REPORT KEY TO THE RUN DATE COMPLETION FILE.
     C*
     C     RNKEY         CHAIN     PRN01                              61
     C     *IN61         IFEQ      '0'
     C                   MOVEL     RDKEY         RNRPNM
     C                   Z-ADD     SPL#          RNSPL#
     C                   MOVE      '1'           RNPASS
     C                   MOVE      *BLANK        RNURNM
     C                   MOVE      X2RNDT        RNRNDT
     C                   UPDATE    PRN01
     C                   ELSE
     C                   MOVE      X2RNDT        RNRNDT
     C                   MOVEL     RDKEY         RNRPNM
     C                   Z-ADD     SPL#          RNSPL#
     C                   MOVE      QQFREQ        RNFREQ
     C                   MOVE      '1'           RNPASS
     C                   MOVE      *BLANK        RNURNM
     C                   WRITE     PRN01
     C                   END
     C*
     C*   READ NEXT USER - REPORT DETAIL RECORD
     C*
     C     NXT           TAG
     C                   READ      PUS01                                  60
     C                   END
     C* PASS NUMBER TWO
     C* THIS READ CYCLE WILL READ BY USER ID
     C* FOR EACH USER/REPORT RECORD
     C* USER BREAK THE SPOOL FILES WILL BE CREATED
     C* FOR THE USER.
     C*
     C     PASS2         TAG
     C*
     C                   MOVE      '2'           XXPASS
     C*
     C                   READ      PUS02                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     USFREQ        IFEQ      QQFREQ
     C     USSTAT        ANDNE     'I'
     C*
     C* CHECK ON USER BREAK
     C     U3URNM        IFNE      SVURNM
     C     FRST          IFEQ      '1'
     C                   MOVE      *BLANKS       FRST
     C                   MOVE      *BLANKS       SVURNM
     C                   MOVEL     U3URNM        SVURNM
     C                   MOVEL     U3URNM        XXUSER
     C                   EXSR      RTE
     C*** CALL PGM TO PRODUCE ROUTING PAGE *****
     C                   CALL      'W4603C'
     C                   PARM                    U3DEST
     C                   PARM                    U3URNM
     C*
     C                   Z-ADD     0             XX#USR            6 0
     C*
     C                   GOTO      CRUN
     C                   END
     C*
     C*
      *** IF WE HAVE PROCESSED 500 OR MORE USERS' REPORTS, THEN WE
      *** WANT TO RESTART THE PROCESS SO WE DON'T EXCEED 9999 SPOOLED FILES
     C     XX#USR        IFGE      500
     C***
     C*** CODE HERE TO UPDAT THE LAST COMPLETED RECORD WITH NXT TO PROCESS *
     C*
     C     RNKEY         CHAIN     PRN01                              61
     C     *IN61         IFEQ      '0'
     C                   MOVEL     U3URNM        RNURNM
     C                   MOVE      '2'           RNPASS
     C                   MOVE      X2RNDT        RNRNDT
     C                   UPDATE    PRN01
     C                   ELSE
     C                   MOVE      X2RNDT        RNRNDT
     C                   MOVE      QQFREQ        RNFREQ
     C                   MOVEL     *BLANKS       RNRPNM
     C                   Z-ADD     *ZEROS        RNSPL#
     C                   MOVE      '2'           RNPASS
     C                   MOVEL     U3URNM        RNURNM
     C                   WRITE     PRN01
     C                   END
     C                   MOVEL     'R'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C                   EXSR      UBRK
     C*
     C*
     C                   ADD       1             XX#USR            6 0
     C*
     C                   EXSR      RTE
     C*** CALL PGM TO PRODUCE ROUTING PAGE *****
     C                   CALL      'W4603C'
     C                   PARM                    U3DEST
     C                   PARM                    U3URNM
     C*
     C                   END
     C*
     C     CRUN          TAG
     C     *IN40         IFEQ      '0'
     C                   MOVE      '1'           *IN40
     C                   END
     C                   MOVE      U3KEY         XXKEY
     C     URKY1         CHAIN     PUR01                              61
     C     URRTCD        IFEQ      '*'
     C*
     C*** GET RD FILE???? FOR OVERLAY DEFINITIONS?
     C     RDKY1         CHAIN     PRD01                              62
     C*
      *** IF AT LEAST ONE SPOOLED FILE IS TO BE CAPTURED FOR THIS USER
      *** WE WILL CALL THE PROGRAM TO PERFORM THE CREATES FOR EACH
      *** SPOOLED FILE BY USER.
      ***
     C     UROF01        IFNE      *BLANKS
     C     UROF02        ORNE      *BLANKS
     C     UROF03        ORNE      *BLANKS
     C     UROF04        ORNE      *BLANKS
     C     UROF05        ORNE      *BLANKS
     C     UROF06        ORNE      *BLANKS
     C     UROF07        ORNE      *BLANKS
     C     UROF08        ORNE      *BLANKS
     C     UROF09        ORNE      *BLANKS
     C     UROF10        ORNE      *BLANKS
     C*
      * ///////////////////////////////////////////
     C                   CALL      'W4601C1'
     C                   PARM                    URRPKY
     C                   PARM                    U3URNM
     C                   PARM                    UROF
     C                   PARM                    RDOL
      *
     C                   ENDIF
     C*
     C                   END
     C*
     C                   END
     C*
     C* REPORT KEY TO THE RUN DATE COMPLETION FILE.
     C*
     C     RNKEY         CHAIN     PRN01                              61
     C     *IN61         IFEQ      '0'
     C                   MOVEL     U3URNM        RNURNM
     C                   MOVE      '2'           RNPASS
     C                   MOVE      X2RNDT        RNRNDT
     C                   UPDATE    PRN01
     C                   ELSE
     C                   MOVE      X2RNDT        RNRNDT
     C                   MOVE      QQFREQ        RNFREQ
     C                   MOVEL     *BLANKS       RNRPNM
     C                   Z-ADD     *ZEROS        RNSPL#
     C                   MOVE      '2'           RNPASS
     C                   MOVEL     U3URNM        RNURNM
     C                   WRITE     PRN01
     C                   END
     C*
     C*
     C                   READ      PUS02                                  60
     C                   END
     C*
     C                   EXSR      UBRK
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    LABEL1
     C                   END
     C                   EXCEPT    LABEL
     C*
     C*   END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*
     C***************************************************************
     C* ROUTING PAGE SUBROUTINE
     C***************************************************************
     C     RTE           BEGSR
     C*
     C                   MOVE      *BLANKS       XDATTN
     C                   MOVE      *BLANKS       XDCOMP
     C                   MOVE      *BLANKS       XDADR1
     C                   MOVE      *BLANKS       XDADR2
     C                   MOVE      *BLANKS       XDCTY
     C                   MOVE      *BLANKS       XDST
     C                   MOVE      *BLANKS       XDZIP1
     C                   MOVE      *BLANKS       XDZIP2
     C*
     C*
     C     U3DEST        IFEQ      3
     C     UDKEY         CHAIN     PUD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UDCOMP        XDCOMP
     C                   MOVEL     UDADR1        XDADR1
     C                   MOVEL     UDADR2        XDADR2
     C                   MOVEL     UDCTY         XDCTY
     C                   MOVE      UDST          XDST
     C                   MOVE      UDZIP1        XDZIP1
     C                   MOVE      UDZIP2        XDZIP2
     C                   MOVEL     UDATTN        XDATTN
     C*
     C     X1ATTN        IFEQ      *BLANKS
     C     X1COMP        ANDEQ     *BLANKS
     C     X1ADR1        ANDEQ     *BLANKS
     C                   MOVEL     UDCOMP        X1COMP           33
     C                   MOVEL     UDADR1        X1ADR1           33
     C                   MOVEL     UDADR2        X1ADR2           33
     C                   MOVEL     UDCTY         X1CTY            20
     C                   MOVE      UDST          X1ST              2
     C                   MOVE      UDZIP1        X1ZIP1            5
     C                   MOVE      UDZIP2        X1ZIP2            4
     C                   MOVEL     UDATTN        X1ATTN           33
     C                   ELSE
     C                   MOVEL     UDCOMP        X2COMP           33
     C                   MOVEL     UDADR1        X2ADR1           33
     C                   MOVEL     UDADR2        X2ADR2           33
     C                   MOVEL     UDCTY         X2CTY            20
     C                   MOVE      UDST          X2ST              2
     C                   MOVE      UDZIP1        X2ZIP1            5
     C                   MOVE      UDZIP2        X2ZIP2            4
     C                   MOVEL     UDATTN        X2ATTN           33
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    LABEL1
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    LABEL
     C                   MOVE      *BLANKS       X1COMP           33
     C                   MOVE      *BLANKS       X1ADR1           33
     C                   MOVE      *BLANKS       X1ADR2           33
     C                   MOVE      *BLANKS       X1CTY            20
     C                   MOVE      *BLANKS       X1ST              2
     C                   MOVE      *BLANKS       X1ZIP1            5
     C                   MOVE      *BLANKS       X1ZIP2            4
     C                   MOVE      *BLANKS       X1ATTN           33
     C                   MOVE      *BLANKS       X2COMP           33
     C                   MOVE      *BLANKS       X2ADR1           33
     C                   MOVE      *BLANKS       X2ADR2           33
     C                   MOVE      *BLANKS       X2CTY            20
     C                   MOVE      *BLANKS       X2ST              2
     C                   MOVE      *BLANKS       X2ZIP1            5
     C                   MOVE      *BLANKS       X2ZIP2            4
     C                   MOVE      *BLANKS       X2ATTN           33
     C                   END
     C*
     C                   END
     C                   END
     C*
     C                   ENDSR
     C***************************************************************
     C* INITIAL SUBROUTINE
     C***************************************************************
     C     INIT          BEGSR
     C* GET NEXT AVAILABLE SEQUENCE NUMBER
     C     *HIVAL        SETGT     PUR01
     C                   READP     PUR01                                  60
     C                   Z-ADD     URSEQ#        XXSEQ#
     C*          URSEQ#    IFNE 0
     C*                    ADD  1         XXSEQ#
     C*                    END
     C* SET UP SAVE USER NAME
     C                   MOVE      *BLANKS       SVURNM
     C* INITIALIZE VARIABLE FRST (FIRST)
     C                   MOVE      '1'           FRST              1
     C* INITIALIZE SPOOL FILE NUMBER
     C                   Z-ADD     1             SPL#              5 0
     C*
     C                   ENDSR
     C*
     C***************************************************************
     C* USER BREAK SUBROUTINE
     C***************************************************************
     C     UBRK          BEGSR
     C*
     C                   MOVE      *BLANKS       QQURNM
     C                   MOVEL     SVURNM        QQURNM           10
     C                   MOVE      *BLANKS       SVURNM
     C                   MOVEL     U3URNM        SVURNM
     C                   MOVE      *BLANKS       QQRTCD
     C                   MOVE      U3URNM        XXUSER
     C*
     C                   MOVE      0             *IN40
     C*
     C                   ENDSR
     C***************************************************************
     C* CLEAR WORK FILE VARIABLES
     C***************************************************************
     C     CLRWK         BEGSR
     C*
     C                   MOVE      *BLANKS       XXRPKY
     C                   MOVE      *BLANKS       XXSEQ#
     C                   MOVE      *BLANKS       XXPRM
     C                   MOVE      *BLANKS       XXPPGM
     C                   MOVE      *BLANKS       XXPUSR
     C                   MOVE      *BLANKS       XXJOBN
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVE      *BLANKS       XXRPGN
     C                   MOVE      *BLANKS       XXOF01
     C                   MOVE      *BLANKS       XXOF02
     C                   MOVE      *BLANKS       XXOF03
     C                   MOVE      *BLANKS       XXOF04
     C                   MOVE      *BLANKS       XXOF05
     C                   MOVE      *BLANKS       XXOF06
     C                   MOVE      *BLANKS       XXOF07
     C                   MOVE      *BLANKS       XXOF08
     C                   MOVE      *BLANKS       XXOF09
     C                   MOVE      *BLANKS       XXOF10
     C                   MOVE      *BLANKS       XXOF1#
     C                   MOVE      *BLANKS       XXOF2#
     C                   MOVE      *BLANKS       XXOF3#
     C                   MOVE      *BLANKS       XXOF4#
     C                   MOVE      *BLANKS       XXOF5#
     C                   MOVE      *BLANKS       XXOF6#
     C                   MOVE      *BLANKS       XXOF7#
     C                   MOVE      *BLANKS       XXOF8#
     C                   MOVE      *BLANKS       XXOF9#
     C                   MOVE      *BLANKS       XXOF0#
     C                   MOVE      *BLANKS       XXRTCD
     C                   MOVE      *BLANKS       XXMSG
     C*
     C                   ENDSR
     C***************************************************************
     C*  ERROR CODE SUB ROUTINE
     C*  RETURN CODE DEFINITIONS:
     C*     1 = REPORT DEFINITION FOR USER IS MARKED AS INACTIVE
     C*     2 = COPY SPOOLED FILE ENDED IN ERROR
     C*     3 = CALL TO QCAEXEC FAILED
     C*     * = STEP COMPLETED SUCCESSFULLY
     C***************************************************************
     C     ERRCD         BEGSR
     C*
     C                   MOVEL     XXRPKY        URRPKY
     C                   MOVE      XXRNDT        URRNDT
     C                   MOVE      XXSEQ#        URSEQ#
     C                   MOVEA     CM            URPRM
     C                   MOVE      XXPPGM        URPPGM
     C                   MOVE      XXPUSR        URPUSR
     C                   MOVE      XXJOBN        URJOBN
     C                   MOVEL     XXKEY         URKEY
     C                   MOVE      XXRPGN        URRPGN
     C                   MOVE      *BLANKS       UROF
     C                   MOVEL     XXOF1#        UROF01
     C                   MOVEL     XXOF2#        UROF02
     C                   MOVEL     XXOF3#        UROF03
     C                   MOVEL     XXOF4#        UROF04
     C                   MOVEL     XXOF5#        UROF05
     C                   MOVEL     XXOF6#        UROF06
     C                   MOVEL     XXOF7#        UROF07
     C                   MOVEL     XXOF8#        UROF08
     C                   MOVEL     XXOF9#        UROF09
     C                   MOVEL     XXOF0#        UROF10
     C                   MOVEL     XXRTCD        URRTCD
     C                   MOVE      XXMSG         URMSG
     C                   MOVE      Q1USER        URUSER
     C                   MOVE      Q1PGM         URPGM
     C                   MOVE      DSDATE        URDT
     C                   MOVE      DSTIME        URTIME
     C                   MOVE      XXPASS        URPASS
     C*
     C                   WRITE     PUR01
     C*
     C                   EXSR      CLRWK
     C*
     C                   ENDSR
     O*
     OLABL      E            LABEL1            5
     OLABL      E            LABEL          1
     O                       X1ATTN              37
     O                       X2ATTN              78
     O          E            LABEL          1
     O                       X1COMP              37
     O                       X2COMP              78
     O          E            LABEL          1
     O                       X1ADR1              37
     O                       X2ADR1              78
     O          E            LABEL          1
     O                       X1ADR2              37
     O                       X2ADR2              78
     O          E            LABEL          2
     O                       X1CTY               24
     O                                           25 ','
     O                       X1ST                27
     O                       X1ZIP1              33
     O                                           34 '-'
     O                       X1ZIP2              38
     O                       X2CTY               65
     O                                           67 ','
     O                       X2ST                69
     O                       X2ZIP1              75
     O                                           76 '-'
     O                       X2ZIP2              80
