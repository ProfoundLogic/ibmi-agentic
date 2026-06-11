     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE N91DGR - TYPE DG DISCOUNT CODE MAINTENANCE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    ALLOWS ENTRY OF SPECIFIC DG DISCOUNT CODE INFORMATION      *
     H*    (I.E. CODES, AMOUNTS)                                      *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    N0920R                                                     *
     H*    N2330R                                                     *
     H*    N2330R01                                                   *
     H*                                                               *
     H*  PROGRAMMER: C:62               REQ/PROJ: 114MAP              *
     H*  CREATED: 02/02/2001                                          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*****************************************************************
     FN91DGF    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(WDW01S:RRN2)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FNMBDLD    UF A E           K DISK
     FNDCOL3    UF A E           K DISK
     F                                     RENAME(PCO01:PCO02)
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)

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
     C                   KFLD                    KK10A2
     C*
     C     COKY1         KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKEFDT
     C                   KFLD                    KK10A1
     C*
     C     COKY2         KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKEFDT
     C*
     C*  GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C     *LIKE         DEFINE    UPDT          KKEFDT
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    CO10A1        KK10A1
     C     *LIKE         DEFINE    CO10A1        KK10A2
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
     C     GENPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID            4
     C*
     C                   MOVEL(P)  @@CODE        KKCODE
     C                   MOVE      @@EFDT        XXEFDT            8 0
     C*
     C                   Z-ADD     XXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
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
     C                   MOVE      'DG'          SCTYPE
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
     C     COKY          CHAIN     PCO02                              60
     C     *IN60         IFEQ      '0'
     C                   DELETE    PCO02
     C                   MOVE      *BLANKS       SC10A1
     C                   MOVE      *BLANKS       SC10A2
     C                   Z-ADD     *ZEROS        SCFEE
     C                   MOVE      *BLANKS       SCFL02
     C                   MOVE      *BLANKS       SCFL03
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
     C                   Z-ADD     *ZEROS        SCFEE
     C                   MOVE      *BLANKS       SCFL02
     C                   MOVE      *BLANKS       SCFL03
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
     C     SCFEE         ANDEQ     *ZEROS
     C     SCFL02        ANDEQ     *BLANKS
     C     SCFL03        ANDEQ     *BLANKS
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
     C* IF INPATIENT THEN VALUE MUST BE 'INP'.
     C*
     C     SCFL03        IFEQ      'I'
     C     SC10A1        ANDNE     'INP'
     C                   MOVE      '1'           *IN81
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ELSE
     C                   MOVE      '0'           *IN81
     C                   ENDIF                                                  END SCFL03
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
     C                   MOVE      '1'           *IN78
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ELSE
     C                   MOVE      '0'           *IN78
     C                   ENDIF                                                  END SCCPT4
     C*
     C* IF INPATIENT THEN VALUE MUST BE 'INP'.
     C*
     C     SCFL03        IFEQ      'I'
     C     SC10A2        ANDNE     'INP'
     C                   MOVE      '1'           *IN82
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ELSE
     C                   MOVE      '0'           *IN82
     C                   ENDIF                                                  END SCFL03
     C*
     C* AMOUNT CAN'T BE ZERO
     C*
     C     SCFEE         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN74
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ELSE
     C                   MOVE      '0'           *IN74
     C                   ENDIF
     C*
     C* THE P/C CODE MUST BE VALID CODE
     C*
     C                   MOVEL(P)  'DGPC'        KKTBID
     C                   MOVEL(P)  SCFL02        KKKEY
     C     GTKY          CHAIN     PGT02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '0'           *IN75
     C                   ELSE
     C                   MOVE      '1'           *IN75
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ENDIF                                                  END *IN60
     C*
     C* THE I/O CODE MUST BE VALID CODE
     C*
     C                   MOVEL(P)  'DGIO'        KKTBID
     C                   MOVEL(P)  SCFL03        KKKEY
     C     GTKY          CHAIN     PGT02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '0'           *IN76
     C                   ELSE
     C                   MOVE      '1'           *IN76
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ENDIF                                                  END *IN60
     C*
     C* IF IF INPATIENT THEN MUST USE PER DIEM
     C*
     C     SCFL03        IFEQ      'I'
     C     SCFL02        IFEQ      'C'
     C                   MOVE      '1'           *IN79
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ELSE
     C                   MOVE      '0'           *IN79
     C                   ENDIF
     C                   ENDIF
     C*
     C* IF IF OUTPATIENT THEN MUST USE % OF CHARGE
     C*
     C     SCFL03        IFEQ      'O'
     C     SCFL02        IFEQ      'P'
     C                   MOVE      '1'           *IN80
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ELSE
     C                   MOVE      '0'           *IN80
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN71         IFEQ      '0'
     C     *IN72         ANDEQ     '0'
     C     *IN73         ANDEQ     '0'
     C     *IN74         ANDEQ     '0'
     C     *IN75         ANDEQ     '0'
     C     *IN76         ANDEQ     '0'
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
     C                   Z-ADD     COFEE         SCFEE
     C                   MOVEL     COFL02        SCFL02
     C                   MOVEL     COFL03        SCFL03
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
     C                   Z-ADD     *ZEROS        SCFEE
     C                   MOVE      *BLANKS       SCFL02
     C                   MOVE      *BLANKS       SCFL03
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
     C     SCFEE         ANDEQ     *ZEROS
     C     SCFL02        ANDEQ     *BLANKS
     C     SCFL03        ANDEQ     *BLANKS
     C                   GOTO      NXTDC3
     C                   ELSE
     C*
     C                   MOVEL(P)  SC10A1        KK10A1
     C                   MOVEL(P)  SC10A2        KK10A2
     C     COKY          CHAIN     PCO02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     SCCODE        COCODE
     C                   Z-ADD     KKEFDT        COEFDT
     C                   MOVEL(P)  SC10A1        CO10A1
     C                   MOVEL(P)  SC10A2        CO10A2
     C                   Z-ADD     SCFEE         COFEE
     C                   MOVEL(P)  SCFL02        COFL02
     C                   MOVEL(P)  SCFL03        COFL03
     C                   MOVE      SCSTAT        COSTAT
     C                   MOVEL     Q1USER        COUSER
     C                   MOVEL     Q1PGM         COPGM
     C                   Z-ADD     DSTIME        COTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CODT
     C                   UPDATE    PCO02
     C                   ELSE
     C                   CLEAR                   PCO02
     C                   MOVEL     SCCODE        COCODE
     C                   Z-ADD     KKEFDT        COEFDT
     C                   MOVEL(P)  SC10A1        CO10A1
     C                   MOVEL(P)  SC10A2        CO10A2
     C                   Z-ADD     SCFEE         COFEE
     C                   MOVEL(P)  SCFL02        COFL02
     C                   MOVEL(P)  SCFL03        COFL03
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
     C     COKY1         SETLL     PCO02                              60
     C*
     C                   EXSR      LOAD
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'WDW01'       Q2RECF
     C                   GOTO      MAINLN
     C                   ENDIF
     C*
     C****************************************
     C*   F10 = SEARCH                       *
     C****************************************
     C*
     C     *INKJ         IFEQ      '1'
     C                   SELECT
     C     HFLD          WHENEQ    'SCFL02'
     C                   MOVEL(P)  'DGPC'        ##TBID
     C                   CALL      'F2380R'      GENPL
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETOFF                                       KL
     C     SFRRN         CHAIN     WDW01S                             98
     C                   SETON                                        35
     C  N98              UPDATE    WDW01S
     C                   ELSE
     C     SFRRN         CHAIN     WDW01S                             98
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL(P)  GTKEY         SCFL02
     C                   SETON                                        36
     C  N98              UPDATE    WDW01S
     C                   ENDIF
     C     HFLD          WHENEQ    'SCFL03'
     C                   MOVEL(P)  'DGIO'        ##TBID
     C                   CALL      'F2380R'      GENPL
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETOFF                                       KL
     C     SFRRN         CHAIN     WDW01S                             98
     C                   SETON                                        36
     C  N98              UPDATE    WDW01S
     C                   ELSE
     C     SFRRN         CHAIN     WDW01S                             98
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL(P)  GTKEY         SCFL03
     C                   SETON                                        31
     C  N98              UPDATE    WDW01S
     C                   ENDIF
     C                   ENDSL
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
     OPCO02     E            PCO02E
     OPBD01     E            PBD01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE TD
NETWORK DISC: HOSP PER DIEM & % OF CHARGES
