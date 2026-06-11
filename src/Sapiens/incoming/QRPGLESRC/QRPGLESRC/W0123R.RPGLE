     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0123R - GROUP CHECK - VOID/STOP/REVERSAL
     H* ***************************************************************
     H*  DESCRIPTION:  THIS PGM WILL VOID, STOP, OR REVERSE ALL THE   *
     H*                TRANSACTIONS WITHIN A PARTICULAR GROUP CHECK.  *
     H*  CALLED FROM: MENU                                            *
     H*  CALLS TO:    NONE                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 03/08/00 C:15 Project: WC577                                  *
     H*               Added code to check for adequate reserves for   *
     H*               reversals and check the claim pmt amount when   *
     H*               processing stops and voids.                     *
     H*                                                               *
     H* 08/17/99 C:15 Support: WC091/19294                            *
     H*               Removed erroneous edit which caused SCR01 to be *
     H*               redisplayed with no error msg.                  *
     H*                                                               *
     H* 08/30/96 C:47 PROJECT: 68ANE                                  *
     H*               ADDED 'RECREATE CHECK REQUEST' (SCRCR) &        *
     H*               'PMT STATUS' (SCSTS) TO SCR01.                  *
     H* ***************************************************************
     FW0123F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWAGCP     O    E           K DISK
     FWAGCL     IF   E           K DISK
     F                                     RENAME(PGC01:PGC02)
     FWMFNL     IF   E           K DISK
     FWDPAL9    IF   E           K DISK
     F*** BEGIN ADD *** 03/08/00 **************************************
     FWDPALB    IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
     FWARPL     IF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
     FWMCML1    IF   E           K DISK
     F*** END ADD ***** 03/08/00 **************************************
     FWDRPL2    IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWTPSL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PPA01)

     D*** BEGIN ADD *** 03/08/00 **************************************
     D*** END ADD ***** 03/08/00 **************************************
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*** BEGIN ADD *** 03/08/00 **************************************
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)                              Reserve type
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)                              Remaining reserve
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)                              Payment total
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D*** END ADD ***** 03/08/00 **************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND             3 0
     C*
     C*  GROUP CHECK PENDING FILE
     C*
     C     GCKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXDATE            7 0
     C                   KFLD                    SCCHK#
     C*
     C*  CLAIM CHECK PAYMENT DETAIL FILE
     C*
     C     PAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDATE
     C                   KFLD                    SCCHK#
     C*
     C*** BEGIN ADD *** 03/08/00 **************************************
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    SCCHK#
     C*** END ADD ***** 03/08/00 **************************************
     C*
     C*  REQUEST PAYMENT DETAIL FILE
     C*
     C     RPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCCHK#
     C                   KFLD                    XXDATE
     C*
     C*  PAYMENT CODE TABLE FILE
     C*
     C     PCKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY
     C*
     C*** BEGIN ADD *** 03/08/00 **************************************
     C*  Key list for the Claim Master file
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  Key list for the Request Payment file
     C*
     C     R2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*** END ADD ***** 03/08/00 **************************************
     C*----------------------------------------------------------------
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C     *LIKE         DEFINE    PCKEY         @@KEY
     C*** BEGIN ADD *** 03/08/00 **************************************
     C     *LIKE         DEFINE    PAFYR         XXFYR
     C     *LIKE         DEFINE    PACASE        XXCASE
     C     *LIKE         DEFINE    PAAMT         XXAMT
     C     *LIKE         DEFINE    PAAMT         XXREM
     C     *LIKE         DEFINE    PARECD        SVRECD
     C                   MOVE      *BLANKS       XXWK3A            3
     C                   MOVE      *BLANKS       XXWK7A            7
     C                   MOVE      *ZEROS        X                 3 0
     C*** END ADD ***** 03/08/00 **************************************
     C                   MOVE      'N'           SCRCR
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     STRT01        TAG
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      UPCO#         XXCO#             3 0
     C                   MOVE      UPFND         XXFND
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      UPFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C                   MOVEA     '0000'        *IN(41)                        POSITION CURSOR
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3=EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   F12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*   F5 = SEARCH LIST CALL
     C*
     C     *INKE         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'W0123R1'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCTRDT
     C                   PARM                    SCCHK#
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCTRDT        OREQ      *ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C                   SETON                                            42
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   F10 = SEARCH LIST CALL
     C*
     C     *INKJ         IFEQ      '1'
      *
      * PAYMENT STATUS CODE SEARCH
      *
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C     HFLD          IFEQ      'SCSTS'
     C*
     C                   CALL      'W2142R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      *BLANK        @@KEY
     C                   MOVEL(P)  @@KEY         SCSTS
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     @@KEY         OREQ      *BLANK
     C                   MOVE      *ON           *IN44
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *ON           *IN41
     C                   GOTO      STRT
     C*
     C                   END                                                    SCSTS
     C                   END                                                    SCR01
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C     HFLD          IFEQ      'SCTRCD'
     C*
     C                   CALL      'W2141R'
     C                   PARM      '2'           @@CALL
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM                    @@KEY
     C                   MOVEL(P)  @@KEY         SCTRCD
     C     *INKC         CABEQ     *ON           TERM
     C*
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*  F13=CHANGE USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C                   CALL      'W0006R'
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C* ================================================================
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  CHECK FOR VALID ORIGINAL TRANSACTION DATE
     C*
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTRDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK FOR DUPLICATE RECORD IN PENDING FILE
     C*
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C     GCKY1         CHAIN     PGC02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK FOR VALID GROUP CHECKS IN PAYMENT FILE AND TOTAL
     C*
     C                   Z-ADD     0             SCAMT
     C                   MOVE      *BLANK        GRCKFG            1
     C     PAKY1         CHAIN     PPA01                              60
     C     *IN60         DOWEQ     '0'
     C     PAFND         IFEQ      XXFND
     C     PAPGTP        ANDEQ     'G'
     C     PATYPE        ANDEQ     'P'
     C                   ADD       PAAMT         SCAMT
     C                   MOVE      'Y'           GRCKFG
     C     PAADST        IFEQ      ' '
     C                   MOVE      'PAID'        SCCHKS
     C                   ELSE
     C     PAADST        IFEQ      'S'
     C                   MOVE      'STOP'        SCCHKS
     C                   ELSE
     C     PAADST        IFEQ      'V'
     C                   MOVE      'VOID'        SCCHKS
     C                   END
     C                   END
     C                   END
     C                   END
     C     PAKY1         READE     PPA01                                  60
     C                   END
     C*
     C     GRCKFG        IFNE      'Y'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
      *
      *  VALIDATE RECREATE CHECK REQUEST FIELD
      *
     C     SCRCR         IFNE      'Y'
     C     SCRCR         ANDNE     'N'
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
     C                   END
      *
      *  VALIDATE PAYMENT STATUS CODE
      *
     C     SCRCR         IFEQ      'Y'
      *
     C*** BEGIN DLT *** 08/17/99 **************************************
     C*  VALIDATE CURRENT STATUS OF CHECKS
     C**********
     C********** PAADST    IFEQ 'V'                        CAN NOT BE VOID
     C********** PAADST    OREQ 'S'                         OR STOP
     C**********           MOVE *ON       *IN29
     C**********           GOTO STRT
     C**********           END
     C*** END DLT ***** 08/17/99 **************************************
      *
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCSTS         XXKEY
     C     TBKEY         CHAIN     PPS01                              60
     C     SCSTS         IFEQ      *BLANKS
     C     *IN60         OREQ      '1'
     C     PSSTAT        OREQ      'I'
     C                   MOVE      '1'           *IN47                          INVALID PMT STS
     C                   GOTO      STRT
     C                   END
      *
     C     PSTYPE        IFNE      'P'
     C                   MOVE      '1'           *IN48                          MUST BE PEND
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C*
     C*  GET PAYEE NAME
     C*
     C                   MOVE      *BLANKS       SCPNAM
     C     RPKEY         CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C     RPFND         IFEQ      XXFND
     C                   MOVEL     RPPNAM        SCPNAM
     C                   LEAVE
     C                   ENDIF
     C     RPKEY         READE     PRP01                                  60
     C                   ENDDO
     C*
     C                   Z-ADD     PAFEIN        SCFEIN
     C                   Z-ADD     PAMOD         SCMOD
     C                   MOVE      *BLANK        SCTRCD
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C* ================================================================
     C     SCR02T        TAG
     C*
     C*  VALID TRAN CODE STATUS AND TYPE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCTRCD        XXKEY
     C     PCKY1         CHAIN     PPC01                              40
     C     *IN40         IFEQ      '0'
     C     PCSTAT        IFNE      'A'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   ELSE
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C     SCCHKS        IFEQ      'PAID'
     C     PCTRTP        IFNE      'V'
     C     PCTRTP        ANDNE     'S'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C                   END
     C     SCCHKS        IFEQ      'VOID'
     C     SCCHKS        OREQ      'STOP'
     C     PCTRTP        IFNE      'Z'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*** BEGIN ADD *** 03/08/00 **************************************
     C*  Process reserve edits.
     C*
     C                   EXSR      EDTRSV
     C*** END ADD ***** 03/08/00 **************************************
     C*
     C*  WE HAVE PASSED ALL EDITS, WRITE TO VOID/STOP/REVERSAL FILE
     C*
     C                   Z-ADD     XXCO#         GCCO#
     C                   Z-ADD     XXFND         GCFND
     C                   Z-ADD     XXDATE        GCTRDT
     C                   Z-ADD     SCCHK#        GCCHK#
     C                   Z-ADD     SCAMT         GCAMT
     C                   Z-ADD     SCFEIN        GCFEIN
     C                   Z-ADD     SCMOD         GCMOD
     C                   MOVEL     SCPNAM        GCPNAM
     C                   MOVEL     SCCHKS        GCCHKS
     C                   MOVEL     SCTRCD        GCTRCD
     C                   MOVE      PCTRTP        GCTRTY
     C                   MOVE      'A'           GCSTAT
     C                   MOVE      Q1USER        GCUSER
     C                   MOVE      Q1PGM         GCPGM
     C                   Z-ADD     DSTIME        GCTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GCDT
     C                   MOVEL     SCRCR         GCRCR
     C                   MOVEL     SCSTS         GCSTS
     C                   MOVE      Q1USER        GCIUSR
     C                   MOVE      Q1PGM         GCIPGM
     C                   Z-ADD     DSTIME        GCITIM
     C                   Z-ADD     GCDT          GCIDT
     C                   WRITE     PGC01
      *
     C                   Z-ADD     0             SCTRDT
     C                   Z-ADD     0             SCCHK#
     C                   MOVE      'N'           SCRCR
     C                   MOVE      *BLANK        SCSTS
     C                   MOVE      *BLANK        SCTRCD
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*** BEGIN ADD *** 03/08/00 **************************************
     C*================================================================
     C* EDTRSV - When processing a "reverse", this routine will
     C*   determine if the reserves on the claims are adequate.  When
     C*   processing a "stop" or "void", this routine will determine
     C*   if the payment amount on the claims are sufficient to prevent
     C*   a negative pmt situation if the transaction is processed.
     C*================================================================
     C     EDTRSV        BEGSR
     C*
     C*  Read all payments associated with the group check.
     C*
     C     PAKY1         CHAIN     PPA01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     PAFND         IFEQ      XXFND
     C     PATYPE        ANDEQ     'P'
     C                   Z-ADD     PAFYR         XXFYR
     C                   Z-ADD     PACASE        XXCASE
     C                   MOVEL     PARECD        SVRECD
     C*
     C*  Retrieve the claim master record.
     C*
     C                   CLEAR                   RR
     C                   CLEAR                   AR
     C     CMKY          CHAIN     PCM01                              99
     C*
     C*  Retrieve the total amount of this check for this claim.
     C*
     C                   Z-ADD     0             XXAMT
     C                   Z-ADD     0             XXREM
     C     PAKY          CHAIN     PPA02                              99
     C     *IN99         DOWEQ     *OFF
     C     PATYPE        IFEQ      'P'
     C     PARECD        ANDEQ     SVRECD
     C                   ADD       PAAMT         XXAMT
     C                   ENDIF
     C     PAKY          READE     PPA02                                  99
     C                   ENDDO
     C*
     C                   Z-ADD     1             X
     C     SVRECD        LOOKUP    RT(X)                                  60
     C                   Z-ADD     RR(X)         XXREM
     C*
     C*  If the claim is open and the request is a reversal, subtract
     C*    any released pmt requests from the remaining reserve then
     C*    verify that the reserves are greater than or equal to the
     C*    total of the reversal.
     C*
     C     CMSTAT        IFEQ      'O'
     C     PCTRTP        ANDEQ     'Z'
     C*
     C     R2KY          CHAIN     PRP02                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     SVRECD        IFEQ      RPRECD
     C     RPSTAT        ANDEQ     ' '
     C                   SUB       RPAMT         XXREM
     C                   ENDIF
     C*
     C     R2KY          READE     PRP02                                  60
     C                   ENDDO
     C*
     C     XXAMT         IFGT      XXREM
     C                   MOVE      XXFYR         XXWK3A
     C                   MOVE      XXCASE        XXWK7A
     C                   MOVEL(P)  XXFND         SCYRCS
     C                   CAT       '-':0         SCYRCS
     C                   CAT       XXWK3A:0      SCYRCS
     C                   CAT       '-':0         SCYRCS
     C                   CAT       XXWK7A:0      SCYRCS
     C     XXAMT         SUB       XXREM         SCAMT1
     C                   EXFMT     WDW01
     C                   GOTO      STRT
     C                   ENDIF                                                  XXAMT>XXREM
     C*
     C                   ENDIF                                                  CMSTAT='O'
     C*
     C*  If request is a void or stop, the payments on the claim must
     C*    be greater than or equal to the transaction amount.
     C*
     C     PCTRTP        IFEQ      'V'
     C     PCTRTP        OREQ      'S'
     C*
     C     XXAMT         IFGT      AR(X)
     C                   MOVE      XXFYR         XXWK3A
     C                   MOVE      XXCASE        XXWK7A
     C                   MOVEL(P)  XXFND         SCYRCS
     C                   CAT       '-':0         SCYRCS
     C                   CAT       XXWK3A:0      SCYRCS
     C                   CAT       '-':0         SCYRCS
     C                   CAT       XXWK7A:0      SCYRCS
     C                   Z-ADD     AR(X)         SCAMT1
     C                   Z-ADD     XXAMT         SCAMT2
     C                   EXFMT     WDW02
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF                                                  PCTRTP=V or S
     C*
     C*  Retrieve next payment record.
     C*
     C                   ENDIF                                                  PAFND=XXFND
     C     PAKY1         READE     PPA01                                  99
     C                   ENDDO
     C*
     C                   ENDSR
     C*** END ADD ***** 03/08/00 **************************************
