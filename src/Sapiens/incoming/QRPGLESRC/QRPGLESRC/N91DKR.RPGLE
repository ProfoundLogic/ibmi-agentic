     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE N91DKR - TYPE DK DISCOUNT CODE MAINTENANCE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    ALLOWS ENTRY OF SPECIFIC DK DISCOUNT CODE INFORMATION      *
     H*    (I.E. CODES, AMOUNTS)                                      *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    N2330R                                                     *
     H*    N2330R01                                                   *
     H*                                                               *
     H*  PROGRAMMER: C:62               REQ/PROJ: 114MJD              *
     H*  CREATED: 03/21/2005                                          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  02/21/08 C:183 Project: 114MPX                               *
     H*                 Call FA753R to write Tracking Record.         *
     H*                                                               *
     H*****************************************************************
     FN91DKF    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(WDW01S:RRN2)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FNMBDLD    UF A E           K DISK
     FNDCOL5    UF A E           K DISK
     F                                     RENAME(PCO01:PCO02)

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D TD              S             70    DIM(1) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  CURSOR               370    371B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  BILILNG CODE TABLE FILE
     C*
     C     BDKY1         KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKEFDT
     C*
     C*  CARVE-OUT TRANSACTION CODE FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKEFDT
     C                   KFLD                    KK10A1
     C                   KFLD                    KK10A3
     C                   KFLD                    KK10A2
     C                   KFLD                    KK10A4
     C*
     C     COKY1         KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKEFDT
     C                   KFLD                    KK10A1
     C                   KFLD                    KK10A3
     C*
     C     COKY2         KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKEFDT
     C*
     C     *LIKE         DEFINE    UPDT          KKEFDT
     C     *LIKE         DEFINE    CO10A1        KK10A1
     C     *LIKE         DEFINE    CO10A2        KK10A2
     C     *LIKE         DEFINE    CO10A3        KK10A3
     C     *LIKE         DEFINE    CO10A4        KK10A4
     C     *LIKE         DEFINE    COCODE        KKCODE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@PRDS           40
     C                   PARM                    @@CALL            1
     C                   PARM                    @@CODE            3
     C                   PARM                    @@EFDT            8
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
     C                   MOVEL(P)  @@CODE        KKCODE
     C                   MOVE      @@EFDT        XXEFDT            8 0
     C*
     C                   Z-ADD     XXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C*** BEG ADD *** 02/21/08 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** END ADD *** 02/21/08 **************************************
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
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
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      @@PRDS        SCPRDS
     C     @@CALL        COMP      '1'                                    09
     C*
     C*** MOVE IN VALUES FROM PARMS TO SCREEN.
     C*
     C                   MOVE      'DK'          SCTYPE
     C                   MOVEL     TD(1)         SCTDSC
     C                   MOVE      *BLANKS       KK10A1
     C*
     C*** GET RECORD IF IT EXISTS, ELSE ITS AN ADD
     C*
     C     BDKY1         CHAIN     PBD01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PBD01E
     C                   MOVE      BDCODE        SCCODE
     C                   MOVE      BDSTAT        SCSTAT
     C                   MOVE      BDCOM1        SCCOM1
     C                   MOVE      BDCOM2        SCCOM2
     C*
     C                   Z-ADD     BDEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVE      '0'           *IN41                          UPDATE EXISTING
     C                   MOVE      UA(2)         SCMODE                         RECORD
     C*
     C                   ELSE
     C                   MOVE      @@CODE        SCCODE
     C                   MOVE      *BLANKS       SCCOM1
     C                   MOVE      *BLANKS       SCCOM2
     C                   Z-ADD     KKEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVE      '1'           *IN41                          ADD RECORD
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      'A'           SCSTAT
     C*
     C                   END
     C*
     C*** BEG ADD *** 02/21/08 **************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      SCCOM1        HDCOM1           60
     C                   MOVE      SCCOM2        HDCOM2           60
     C                   ENDIF                                                  CLTID IFEQ 114
     C*** END ADD *** 02/21/08 **************************************
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*===============================================================
     C* INITIALIZATION    AREA
     C*
     C     TOP           TAG
     C                   EXSR      INZSR
     C*
     C*===============================================================
     C*
     C     MAINLN        TAG
     C*
     C*===============================================================
     C* SCREEN EXECUTION AREA
     C*
     C                   EXSR      SCREXC
     C*
     C*===============================================================
     C* HANDLE F KEYS IF PRESSED
     C*
     C     *IN11         CASEQ     '1'           FKEYS
     C                   END                                                      END *IN11
     C*
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C*
     C     SCPST1        IFNE      *BLANKS
     C*
     C  N09              EXSR      EDITDC
     C*
     C                   EXSR      WRITDC
     C*
     C                   MOVEL(P)  SCPST1        KK10A1
     C                   GOTO      TOP
     C                   END                                                    END SCPST1
     C*
     C                   Z-ADD     RRN2          SVRRN2            5 0
     C*
     C                   READC     WDW01S                                 50
     C     *IN50         IFEQ      '0'
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** SAVE THAT DATA TO BE PASSED BACK TO THE CALLING PROGRAM
     C     SCSEL         IFEQ      '4'
     C                   EXSR      DELDSC
     C                   ELSE                                                   ELSE EQ '1'
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '4' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   Z-ADD     RRN2          #@$2
     C                   UPDATE    WDW01S
     C                   MOVE      *BLANK        SCSEL
     C                   GOTO      MAINLN
     C                   END                                                    END NE ' '
     C                   END                                                    END EQ '4'
     C                   END                                                    END 0 *IN50
     C*
     C  N09              EXSR      EDITDC
     C                   EXSR      WRITDC
     C*
     C*===============================================================
     C* TERMINATION OF PROGRAM
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
     C/EJECT
     C*====================================================================*
     C* * *   DELDSC SUBROUTINE                              D E L D S C ***
     C*====================================================================*
     C     DELDSC        BEGSR
     C*
     C                   MOVE      RRN2          SRRN2             5 0
     C                   Z-ADD     *ZEROS        K                 5 0
     C     NXTDC4        TAG
     C                   ADD       1             K
     C     K             CHAIN     WDW01S                             98
     C     *IN98         IFEQ      '0'
     C*
     C     SCSEL         IFNE      *BLANKS
     C     SCSEL         ANDNE     '4'
     C                   MOVE      '1'           *IN25
     C                   UPDATE    WDW01S
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ELSE
     C                   MOVE      '0'           *IN25
     C                   END                                                    END SCSEL
     C*
     C     SCSEL         IFEQ      '4'
     C                   MOVEL(P)  SC10A1        KK10A1
     C                   MOVEL(P)  SC10A2        KK10A2
     C                   MOVEL(P)  SC10A3        KK10A3
     C                   MOVEL(P)  SC10A4        KK10A4
     C     COKY          CHAIN     PCO02                              60
     C     *IN60         IFEQ      '0'
     C                   DELETE    PCO02
     C*** BEG ADD *** 02/21/08 **************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      'D'           @UWFL1
     C                   EXSR      WRTTR1
     C                   ENDIF                                                  CLTID IFEQ 114
     C*** END ADD *** 02/21/08 **************************************
     C                   MOVE      *BLANKS       SC10A1
     C                   MOVE      *BLANKS       SC10A2
     C                   MOVE      *BLANKS       SC10A3
     C                   MOVE      *BLANKS       SC10A4
     C                   Z-ADD     *ZEROS        SCFEE
     C                   MOVE      *BLANKS       SCDEL
     C     '* DELETE'    CAT       'D'           XXDEL             9
     C                   MOVEL     XXDEL         SCDEL
     C                   MOVE      '1'           *IN08
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ELSE
     C                   MOVE      *BLANKS       SC10A1
     C                   MOVE      *BLANKS       SC10A2
     C                   MOVE      *BLANKS       SC10A3
     C                   MOVE      *BLANKS       SC10A4
     C                   Z-ADD     *ZEROS        SCFEE
     C                   MOVE      *BLANKS       SCDEL
     C     '* DELETE'    CAT       'D'           XXDEL             9
     C                   MOVEL     XXDEL         SCDEL
     C                   MOVE      '1'           *IN08
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   END
     C                   ELSE
     C     *IN25         IFEQ      '0'
     C                   UPDATE    WDW01S
     C                   END
     C                   END
     C*
     C                   GOTO      NXTDC4
     C                   END                                                    END *IN98
     C*
     C                   ENDSR
     C/EJECT
     C*====================================================================*
     C* * *   EDITDC SUBROUTINE                               E D I T D C ***
     C*====================================================================*
     C     EDITDC        BEGSR
     C*
     C*  THE BILLING CODE CANNOT BLANK
     C*
     C     SCCOM1        IFEQ      *BLANK
     C     SCCOM2        ANDEQ     *BLANK
     C                   MOVE      '1'           *IN77
     C                   MOVE      '1'           *IN77
     C                   GOTO      MAINLN
     C                   END
     C*
     C                   MOVE      RRN2          SRRN2             5 0
     C                   Z-ADD     *ZEROS        K                 5 0
     C     NXTDC2        TAG
     C                   ADD       1             K
     C     K             CHAIN     WDW01S                             98
     C     *IN98         IFEQ      '0'
     C*
     C     SC10A1        IFEQ      *BLANKS
     C     SC10A2        ANDEQ     *BLANKS
     C     SC10A3        ANDEQ     *BLANKS
     C     SC10A4        ANDEQ     *BLANKS
     C     SCFEE         ANDEQ     *ZEROS
     C                   GOTO      NXTDC2
     C                   ELSE
     C*
     C* IF ANYTHING ON LINE ENTERED FROM CODE MUST BE ENTERED
     C*
     C     SC10A1        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN71
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ELSE
     C                   MOVE      '0'           *IN71
     C                   ENDIF                                                  END SCCPT4
     C*
     C* IF ANYTHING ON LINE ENTERED TO CODE MUST BE ENTERED
     C*
     C     SC10A2        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN72
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ELSE
     C                   MOVE      '0'           *IN72
     C                   ENDIF                                                  END SCCPT4
     C*
     C* IF ANYTHING ON LINE ENTERED TO CODE MUST BE > THAN FROM CODE
     C*
     C     SC10A2        IFLT      SC10A1
     C                   MOVE      '1'           *IN73
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ELSE
     C                   MOVE      '0'           *IN73
     C                   ENDIF                                                  END SCCPT4
     C*
     C* IF ONE CODE ENTERED THEN THE OTHER CODE MUST BE ENTERED
     C*
     C     SC10A3        IFEQ      *BLANKS
     C     SC10A4        ANDNE     *BLANKS
     C     SC10A3        ORNE      *BLANKS
     C     SC10A4        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN74
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ELSE
     C                   MOVE      '0'           *IN74
     C                   ENDIF                                                  END SCCPT4
     C*
     C* IF ANYTHING ON LINE ENTERED TO CODE MUST BE > THAN FROM CODE
     C*
     C     SC10A4        IFLT      SC10A3
     C                   MOVE      '1'           *IN75
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ELSE
     C                   MOVE      '0'           *IN75
     C                   ENDIF                                                  END SCCPT4
     C*
     C* AMOUNT CAN'T BE ZERO
     C*
     C     SCFEE         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN76
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ELSE
     C                   MOVE      '0'           *IN76
     C                   ENDIF
     C*
     C     *IN71         IFEQ      '0'
     C     *IN72         ANDEQ     '0'
     C     *IN73         ANDEQ     '0'
     C     *IN74         ANDEQ     '0'
     C     *IN75         ANDEQ     '0'
     C                   UPDATE    WDW01S
     C                   END
     C*
     C                   GOTO      NXTDC2
     C                   ENDIF                                                  END SC10A1
     C                   ENDIF                                                  END *IN98
     C                   MOVE      SRRN2         RRN2
     C*
     C                   ENDSR
     C/EJECT
     C*====================================================================*
     C* * *   INZSR SUBROUTINE                                    I N I T ***
     C*====================================================================*
     C     INZSR         BEGSR
     C*
     C     COKY          SETLL     PCO02                              60
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     WDW01C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C*
     C                   ENDSR
     C*====================================================================*
     C* * *   LOAD SUBROUTINE                                    L O A D ***
     C*====================================================================*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   MOVE      '0'           *IN08
     C*
     C*   Position Cursor flag - used to check if the 1st subfile RRN
     C*     of the current page needs to be saved.
     C*
     C                   MOVE      'Y'           PCFL              1
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        7             X                 5 0
     C*
     C     COKY2         READE     PCO02                                  60
     C*
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    PCO02E
     C*
     C                   ADD       1             RRN2              5 0
     C                   MOVEL(P)  CO10A1        SC10A1
     C                   MOVEL(P)  CO10A2        SC10A2
     C                   MOVEL(P)  CO10A3        SC10A3
     C                   MOVEL(P)  CO10A4        SC10A4
     C                   Z-ADD     COFEE         SCFEE
     C*** BEG ADD *** 02/21/08 **************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      SCFEE         HDFEE
     C                   ENDIF                                                  CLTID IFEQ 114
     C*** END ADD *** 02/21/08 **************************************
     C                   MOVEL     *BLANKS       SCDEL
     C*
     C*   Save 1st subfile RRN of the current page
     C*
     C     PCFL          IFEQ      'Y'
     C                   Z-ADD     RRN2          XXRRN2            5 0
     C                   MOVE      'N'           PCFL
     C                   END
     C                   WRITE     WDW01S
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       SCSEL
     C                   MOVE      *BLANKS       SC10A1
     C                   MOVE      *BLANKS       SC10A2
     C                   MOVE      *BLANKS       SC10A3
     C                   MOVE      *BLANKS       SC10A4
     C                   Z-ADD     *ZEROS        SCFEE
     C*** BEG ADD *** 02/21/08 **************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      SCFEE         HDFEE
     C                   ENDIF                                                  CLTID IFEQ 114
     C*** END ADD *** 02/21/08 **************************************
     C                   ADD       1             RRN2
     C*
     C*   Save 1st subfile RRN of the current page
     C*
     C     PCFL          IFEQ      'Y'
     C                   Z-ADD     RRN2          XXRRN2
     C                   MOVE      'N'           PCFL
     C                   END
     C                   WRITE     WDW01S
     C*
     C                   END
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C*
     C                   Z-ADD     XXRRN2        #@$2
     C*
     C                   ENDSR
     C/EJECT
     C*===============================================================
     C     SCREXC        BEGSR
     C*===============================================================
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01C'
     C     Q2RECF        OREQ      'WDW01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     WDW01
     C                   EXFMT     WDW01C
     C                   MOVE      '0'           *IN01
     C     SVRRN2        IFNE      0
     C                   Z-ADD     SVRRN2        RRN2
     C                   Z-ADD     0             SVRRN2
     C                   END
     C                   END
     C*
     C*
     C                   ENDSR
     C*
     C/EJECT
     C*====================================================================*
     C* * *   WRITDC SUBROUTINE                              W R I T D C ***
     C*====================================================================*
     C     WRITDC        BEGSR
     C*
     C     BDKY1         CHAIN     PBD01                              97
     C*
     C                   Z-ADD     *ZEROS        K                 5 0
     C     NXTDC3        TAG
     C                   ADD       1             K
     C     K             CHAIN     WDW01S                             98
     C     *IN98         IFEQ      '0'
     C*
     C     SC10A1        IFEQ      *BLANKS
     C     SC10A2        ANDEQ     *BLANKS
     C     SC10A3        ANDEQ     *BLANKS
     C     SC10A4        ANDEQ     *BLANKS
     C     SCFEE         ANDEQ     *ZEROS
     C                   GOTO      NXTDC3
     C                   ELSE
     C*
     C                   MOVEL(P)  SC10A1        KK10A1
     C                   MOVEL(P)  SC10A2        KK10A2
     C                   MOVEL(P)  SC10A3        KK10A3
     C                   MOVEL(P)  SC10A4        KK10A4
     C     COKY          CHAIN     PCO02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     SCCODE        COCODE
     C                   Z-ADD     KKEFDT        COEFDT
     C                   MOVEL(P)  SC10A1        CO10A1
     C                   MOVEL(P)  SC10A2        CO10A2
     C                   MOVEL(P)  SC10A3        CO10A3
     C                   MOVEL(P)  SC10A4        CO10A4
     C                   Z-ADD     SCFEE         COFEE
     C                   MOVE      SCSTAT        COSTAT
     C                   MOVEL     Q1USER        COUSER
     C                   MOVEL     Q1PGM         COPGM
     C                   Z-ADD     DSTIME        COTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CODT
     C                   UPDATE    PCO02
     C*** BEG ADD *** 02/21/08 **************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      'U'           @UWFL1            1
     C                   EXSR      WRTTR1
     C                   ENDIF                                                  CLTID IFEQ 114
     C*** END ADD *** 02/21/08 **************************************
     C                   ELSE
     C                   CLEAR                   PCO02
     C                   MOVEL     SCCODE        COCODE
     C                   Z-ADD     KKEFDT        COEFDT
     C                   MOVEL(P)  SC10A1        CO10A1
     C                   MOVEL(P)  SC10A2        CO10A2
     C                   MOVEL(P)  SC10A3        CO10A3
     C                   MOVEL(P)  SC10A4        CO10A4
     C                   Z-ADD     SCFEE         COFEE
     C                   MOVE      SCSTAT        COSTAT
     C                   MOVEL     Q1USER        COUSER
     C                   MOVEL     Q1PGM         COPGM
     C                   Z-ADD     DSTIME        COTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CODT
     C*
     C                   MOVEL     Q1USER        COIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         COIDT
     C*
     C                   WRITE     PCO02
     C*** BEG ADD *** 02/21/08 **************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      'W'           @UWFL1
     C                   EXSR      WRTTR1
     C                   ENDIF                                                  CLTID IFEQ 114
     C*** END ADD *** 02/21/08 **************************************
     C                   ENDIF
     C                   GOTO      NXTDC3
     C*
     C                   ENDIF                                                  END SCCPT4
     C                   ENDIF                                                  END *IN98
     C*
     C*  ALL EDITS HAVE BEEN PASSED  *** WRITE/UPDATE FILE!
     C*
     C     *IN97         IFEQ      *OFF
     C*
     C                   MOVEL     SCCODE        BDCODE
     C                   MOVE      KKEFDT        BDEFDT
     C                   MOVE      SCSTAT        BDSTAT
     C                   MOVE      SCCOM1        BDCOM1
     C                   MOVE      SCCOM2        BDCOM2
     C                   MOVE      SCTYPE        BDTYPE
     C                   MOVE      'D'           BDRCDT
     C*
     C                   MOVE      Q1USER        BDUSER
     C                   MOVE      Q1PGM         BDPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BDDT
     C*
     C                   Z-ADD     DSTIME        BDTIME
     C                   UPDATE    PBD01
     C*** BEG ADD *** 02/21/08 **************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      'U'           @UWFLG            1
     C                   EXSR      WRTTR
     C                   ENDIF                                                  CLTID IFEQ 114
     C*** END ADD *** 02/21/08 **************************************
     C                   ELSE
     C                   CLEAR                   PBD01
     C*
     C                   MOVEL     SCCODE        BDCODE
     C                   MOVE      KKEFDT        BDEFDT
     C                   MOVE      SCSTAT        BDSTAT
     C                   MOVE      SCCOM1        BDCOM1
     C                   MOVE      SCCOM2        BDCOM2
     C                   MOVE      SCTYPE        BDTYPE
     C                   MOVE      'D'           BDRCDT
     C*
     C                   MOVE      Q1USER        BDUSER
     C                   MOVE      Q1PGM         BDPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BDDT
     C*
     C                   Z-ADD     DSTIME        BDTIME
     C                   MOVE      Q1USER        BDIUSR
     C                   WRITE     PBD01
     C*** BEG ADD *** 02/21/08 **************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      'W'           @UWFLG
     C                   EXSR      WRTTR
     C                   ENDIF                                                  CLTID IFEQ 114
     C*** END ADD *** 02/21/08 **************************************
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C/EJECT
     C*===============================================================
     C     FKEYS         BEGSR
     C*===============================================================
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  END *INKC
     C*
     C     *INKD         IFEQ      '1'
     C                   END
     C*
     C****************************************
     C*   F5 = REFRESH                       *
     C****************************************
     C*
     C     *INKE         IFEQ      '1'
     C*
     C*   Clear subfile
     C*
     C                   MOVE      '1'           *IN04
     C                   WRITE     WDW01C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C                   Z-ADD     0             XXRRN2
     C*
     C                   MOVEL(P)  SCPST1        KK10A1
     C                   MOVEL(P)  SCPST2        KK10A3
     C     COKY1         SETLL     PCO02                              60
     C*
     C                   EXSR      LOAD
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'WDW01'       Q2RECF
     C                   GOTO      MAINLN
     C                   ENDIF
     C*
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01S'
     C     Q2RECF        OREQ      'WDW01C'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF
     C                   ENDIF                                                  END *INKL
     C*
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN37         CABEQ     '1'           MAINLN                         NO ROLL IF EOF
     C                   MOVEL     *BLANKS       KK10A1
     C                   EXSR      LOAD
     C                   GOTO      MAINLN
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C                   GOTO      MAINLN
     C                   ENDSR
     C*** BEG ADD *** 02/21/08 **************************************
     C*================================================================
     C* WRTTR - Write Tracking Record for NMBDP
     C*================================================================
     C     WRTTR         BEGSR
      *
     C                   SELECT
      *
     C     @UWFLG        WHENEQ    'U'
      *
     C     HDCOM1        IFNE      BDCOM1
      *
     C                   MOVEL(P)  'BDCOM1'      @@FLD
     C                   MOVEL(P)  HDCOM1        @@FVAL
     C                   MOVEL(P)  BDCOM1        @@TVAL
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE            3
     C                   PARM      BDTYPE        @@TYPE            2
     C                   PARM      BDRCDT        @@RCDT            1
     C                   PARM      BDEFDT        @EFDT1            7 0
     C                   PARM      *BLANKS       @@CPT4            5
     C                   PARM      *BLANKS       @@PMOD            2
     C                   PARM      *BLANKS       @@10A1           10
     C                   PARM      *BLANKS       @@10A2           10
     C                   PARM      *BLANKS       @@10A3           10
     C                   PARM      *BLANKS       @@10A4           10
     C                   PARM                    @@FLD            10
     C                   PARM                    @@FVAL           80
     C                   PARM                    @@TVAL           80
     C                   ENDIF                                                  HDCOM2 IFNE BDCOM2
      *
     C     HDCOM2        IFNE      BDCOM2
      *
     C                   MOVEL(P)  'BDCOM2'      @@FLD
     C                   MOVEL(P)  HDCOM2        @@FVAL
     C                   MOVEL(P)  BDCOM2        @@TVAL
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @EFDT1
     C                   PARM      *BLANKS       @@CPT4
     C                   PARM      *BLANKS       @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDCOM2 IFNE BDCOM2
      *
     C     @UWFLG        WHENEQ    'W'
      *
     C                   MOVEL(P)  'Added'       @@FLD
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @EFDT1
     C                   PARM      *BLANKS       @@CPT4
     C                   PARM      *BLANKS       @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  WRTTR
     C*================================================================
     C* WRTTR1 - Write Tracking Record for NDCOP
     C*================================================================
     C     WRTTR1        BEGSR
      *
     C                   SELECT
      *
     C     @UWFL1        WHENEQ    'U'
      *
     C     HDFEE         IFNE      COFEE
      *
     C                   MOVEL(P)  'COFEE'       @@FLD
     C                   MOVEL(P)  HDFEE         @@FVAL
     C                   MOVEL(P)  COFEE         @@TVAL
      *
     C                   CALL      'FA753R'
     C                   PARM      SCCODE        @@CODE
     C                   PARM      SCTYPE        @@TYPE
     C                   PARM      'D'           @@RCDT
     C                   PARM      COEFDT        @EFDT1
     C                   PARM      *BLANKS       @@CPT4
     C                   PARM      *BLANKS       @@PMOD
     C                   PARM      CO10A1        @@10A1
     C                   PARM      CO10A2        @@10A2
     C                   PARM      CO10A3        @@10A3
     C                   PARM      CO10A4        @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDFEE IFNE COFEE
      *
     C     @UWFL1        WHENEQ    'W'
      *
     C                   MOVEL(P)  'Added'       @@FLD
      *
     C                   CALL      'FA753R'
     C                   PARM      SCCODE        @@CODE
     C                   PARM      SCTYPE        @@TYPE
     C                   PARM      'D'           @@RCDT
     C                   PARM      COEFDT        @EFDT1
     C                   PARM      *BLANKS       @@CPT4
     C                   PARM      *BLANKS       @@PMOD
     C                   PARM      CO10A1        @@10A1
     C                   PARM      CO10A2        @@10A2
     C                   PARM      CO10A3        @@10A3
     C                   PARM      CO10A4        @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
      *
     C     @UWFL1        WHENEQ    'D'
      *
     C                   MOVEL(P)  'Deleted'     @@FLD
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @EFDT1
     C                   PARM      *BLANKS       @@CPT4
     C                   PARM      *BLANKS       @@PMOD
     C                   PARM      SC10A1        @@10A1
     C                   PARM      SC10A2        @@10A2
     C                   PARM      SC10A3        @@10A3
     C                   PARM      SC10A4        @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  WRTTR1
     C*** END ADD *** 02/21/08 **************************************
     OPCO02     E            PCO02E
     OPBD01     E            PBD01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE TD
NETWORK DISC: MADIGAN DISCOUNT
