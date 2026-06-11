     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS223R - DISPLAY DOCUMENT DETAIL
     H******************************************************************
      * REVISIONS:
      *
     F* 07/09/07 C:191 PROJECT: 10IJI                                *
     F*               -DISPLAY RECEIVED DATE (SCRCVD) WHEN SYSTEM    *
     F*                CONTROL FLAG IS SET TO "Y".                   *
     H******************************************************************
     H* INDICATOR USAGE                                                *
     H*  40       CHAIN TO IMBML                                       *
      *  98       TO CONTROL DISPLAY OF RECEIVED DATE             DSPF *
     H******************************************************************
     FIS223F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F*** BEG ADD ********** 07/09/07 ********************************
     F*** LOGICAL OF SDRPF
     FSSYLF     IF   E           K DISK
     F*** END ADD ********** 07/09/07 ********************************
     FIMDOL1    IF   E           K DISK
     FITDSL     IF   E           K DISK
     FIMDTL     IF   E           K DISK
     FIMBML     IF   E           K DISK
      *
      *

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDCID           12
     C                   PARM                    QQRTCD            1
     C*
     C*
     C     DOKY          KLIST
     C                   KFLD                    QQDCID
     C*
     C*
     C     DSKY          KLIST
     C                   KFLD                    DODSTS
     C*
     C*
     C     BMKY          KLIST
     C                   KFLD                    DOBCHI
     C*
     C*
     C     DTKY          KLIST
     C                   KFLD                    DODTYP
     *** BEG ADD ********** 07/09/07 ********************************
     * KEY LIST FOR THE SYSTEM CONTROL FILE
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     *** END ADD ********** 07/09/07 ********************************
     C*
     C* INITIALIZATION SECTION
     C*
     C                   TIME                    DSDTTM
     C*
      /Free
       scPrDs = 'Document Details';
      /End-Free
     C*
     C*
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     *** BEG ADD ********** 07/09/07 ********************************
     C                   MOVEL     'SC'          XXSYK
     C     SYKY          CHAIN     SDR01                              60
     C     *IN60         IFEQ      *OFF
     C     DRFL15        IFEQ      'Y'
     C                   SETON                                        98
     C                   ENDIF                                                  END DRFL15
     C                   ENDIF                                                  END *IN60
     *** END ADD ********** 07/09/07 ********************************
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     DOKY          CHAIN     PDO01                              40
     C     *IN40         IFEQ      '0'
     C*
     C*** THE DOCUMENT RECORD   HERE ***
     C*** GET DOC   STATUS DESCRIPTION
     C*
     *** BEG ADD ********** 07/09/07 ********************************
     C     *IN98         IFEQ      *ON
     C     DORCVD        IFNE      0
     C                   MOVE      DORCVD        ##RCVD            8 0
     C                   CALL      'ISCVTDT'
     C                   PARM                    ##RCVD
     C                   PARM      '*YYMD'       ##FFMT            8
     C                   PARM      '*MDYY'       ##TFMT            8
     C                   PARM                    ##DTO            10
     C                   PARM      *BLANKS       ##ERR             1
     C     ##ERR         IFNE      '1'
     C                   MOVEL     ##DTO         SCRCVD
     C                   ENDIF                                                  END DORCVD
     C                   ENDIF                                                  END ##ERR
     C                   ENDIF                                                  END *IN98
     *** END ADD ********** 07/09/07 ********************************
     C     DSKY          CHAIN     PDS01                              41
     C     *IN41         IFEQ      *OFF
     C                   MOVEL     DSLDSC        SCSDSC
     C                   ELSE
     C                   MOVEL     *BLANKS       SCSDSC
     C                   ENDIF
     C*
     C*** GET BATCH DESC  HERE
     C*
     C     BMKY          CHAIN     PBM01                              41
     C     *IN41         IFEQ      *OFF
     C                   MOVEL     BMBDSC        SCBDSC
     C                   ELSE
     C                   MOVEL     *BLANKS       SCBDSC
     C                   ENDIF
     C*
     C*** GET DOC TYPE DESCRIPTION ***
     C*
     C     DTKY          CHAIN     PDT01                              41
     C     *IN41         IFEQ      *OFF
     C                   MOVEL     DTDESC        SCDDSC
     C                   ELSE
     C                   MOVEL     *BLANKS       SCDDSC
     C                   ENDIF
     C*
     C*
     C                   MOVE      DOSDT         XXSDT             8
     C*
     C                   MOVEL     XXSDT         XXSY              4
     C                   MOVE      XXSDT         XXSD              2
     C     2             SUBST     XXSDT:5       XXSM              2
     C     XXSM          CAT       XXSD          XXMD              4
     C     XXMD          CAT       XXSY          XXSDT
     C*
     C                   MOVE      XXSDT         SCSDT
     C*
     C*
     C                   MOVE      DOSTMD        SCSTM
     C*
     C*
     C                   ELSE
     C                   GOTO      TERM
     C*
     C                   END
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C**********************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C********************************************
     C*   F3 = EXIT                              *
     C********************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*  F7 = DOCUMENT NOTES DISPLAY        *
     C****************************************
     C     *INKG         IFEQ      '1'
     C*
     C                   MOVEL     'IS223R  '    #7PNAM           10
     C                   CALL      'IS016R'
     C                   PARM                    QQDCID
     C                   PARM                    #7PNAM
     C*
     C*
     C                   GOTO      STRT
     C                   END                                                    END *INKF
     C********************************************
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C                   MOVEL     'B'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKL
     C********************************************
     C*   *IN10 = PROCESS HELP REQUESTS          *
     C********************************************
     C  N10
     CAN 10*IN10         IFEQ      '1'
     C  N10
     CAN 10              MOVE      '1'           *IN10
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C********************************************
     C                   END                                                    END CMDKEYS
     C*************************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*
     C*
     C                   GOTO      STRT                                         REDISPLAY
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
