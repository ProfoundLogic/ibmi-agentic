     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE N91DER - TYPE DE DISCOUNT CODE MAINTENANCE
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program maintains the Anthem network code.         **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     N0910R - Network Provider Discount Code Maintenance     **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     F0510R - Diag Code by Code Srch - Scrn Pgm              **
     H**     F0520R - CPT-4 Proc Code Srch - Scrn Pgm                **
     H**                                                             **
     H**   PROGRAMMER: 31                                            **
     H**   CREATED: 01/15/98         PROJECT: 60MBS                  **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  09/20/00 C:62  Project: NW068
     H*                 -Remove F20=Change status
     H*                 -Remove F23=Delete
     H*
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      22       CPT4 can not be blank                         **
     H**      23       CPT4 code selected is invalid                 **
     H**      25       Invalid option                                **
     H**      28       Invalid type code                             **
     H**      35       Position cursor at Amount                     **
     H**      36       Position cursor at CPT4                       **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**   NMBDLD   (PBD01): Network Billing Rate/Discount Master    **
     H**                     (CODE/EFDT/RCDT='D')                    **
     H**   NDCOL1   (PCO01): Carve-out Transaction (CODE/CPT4/EFDT)  **
     H**   SMCOL    (PCO02): System Company File (CO#)               **
     H**   SMUPL    (PUP01): User Parameter File (USR)               **
     H**   FMPRLUC  (PPR01): Proceedure Code File (CO#/CPT4/EFDT)    **
     H**   FMDIL    (PDI01): Diagnosis Code File (ICD9/LOST)         **
     H**   FMUPL    (PUP01): User Parameter File (USR)               **
     H*****************************************************************
     H*
     FN91DEF    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FNMBDLD    UF A E           K DISK
     FNDCOL     UF A E           K DISK
     F                                     RENAME(PCO01:PCO02)
     FFMPRLUC   IF   E           K DISK
     FFMDIL     IF   E           K DISK

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
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  BILILNG CODE TABLE FILE
     C*
     C     BDKY1         KLIST
     C                   KFLD                    QQCODE
     C                   KFLD                    YMEFDT
     C*
     C*  CARVE-OUT TRANSACTION CODE FILE
     C*
     C     COKY1         KLIST
     C                   KFLD                    QQCODE
     C                   KFLD                    YMEFDT
     C                   KFLD                    SCCPT4
     C*
     C     COKY          KLIST
     C                   KFLD                    QQCODE
     C                   KFLD                    YMEFDT
     C                   KFLD                    XXRCDE            5
     C*
     C     COKY2         KLIST
     C                   KFLD                    QQCODE
     C                   KFLD                    YMEFDT
     C*
     C     DIKEY         KLIST
     C                   KFLD                    XXICD9            5
     C                   KFLD                    XXLOST            1
     C*
     C*  CPT4 PROCEDURE CODE FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXCPT4            5
     C                   KFLD                    YMEFDT
     C*
     C*  ICD9 CODE FILE
     C*
     C     *LIKE         DEFINE    UPDT          YMEFDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCODE            3
     C                   PARM                    QQEFDT            8
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
     C                   MOVE      QQEFDT        XXEFDT            8 0
     C*
     C                   Z-ADD     XXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         YMEFDT
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
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
     C                   MOVE      'D'           XXLOST
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*** MOVE IN VALUES FROM PARMS TO SCREEN.
     C*
     C                   MOVE      'DE'          SCTYPE
     C                   MOVEL     TD(1)         SCTDSC
     C                   MOVE      *BLANKS       SCRCDE
     C                   MOVE      *BLANKS       XXRCDE
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
     C     SCSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN64
     C                   ELSE
     C                   MOVE      '0'           *IN64
     C                   END
     C*
     C                   ELSE
     C                   MOVE      QQCODE        SCCODE
     C                   MOVE      *BLANKS       SCCOM1
     C                   MOVE      *BLANKS       SCCOM2
     C                   Z-ADD     YMEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVE      '1'           *IN41                          ADD RECORD
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      '0'           *IN64
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
     C     SCRCDE        IFNE      *BLANKS
     C*
     C                   EXSR      EDITDC
     C*
     C                   EXSR      WRITDC
     C*
     C                   MOVEL     SCRCDE        XXRCDE
     C                   MOVE      *BLANKS       SCRCDE
     C                   GOTO      TOP
     C                   END                                                    END SCRCDE
     C*
     C                   Z-ADD     RRN2          SVRRN2            5 0
     C*
     C                   READC     SCR01S                                 50
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
     C                   UPDATE    SCR01S
     C                   MOVE      *BLANK        SCSEL
     C                   GOTO      MAINLN
     C                   END                                                    END NE ' '
     C                   END                                                    END EQ '4'
     C                   END                                                    END 0 *IN50
     C*
     C                   EXSR      EDITDC
     C*
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
     C     K             CHAIN     SCR01S                             75
     C     *IN75         IFEQ      '0'
     C*
     C     SCSEL         IFNE      *BLANKS
     C     SCSEL         ANDNE     '4'
     C                   MOVE      '1'           *IN25
     C                   UPDATE    SCR01S
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ELSE
     C                   MOVE      '0'           *IN25
     C                   END                                                    END SCSEL
     C*
     C     SCSEL         IFEQ      '4'
     C     COKY1         CHAIN     PCO02                              60
     C     *IN60         IFEQ      '0'
     C                   DELETE    PCO02
     C                   MOVE      *BLANKS       SCCPT4
     C                   Z-ADD     *ZEROS        SCAAMT
     C                   MOVE      *BLANKS       SCTYP
     C                   MOVE      *BLANKS       SCDEL
     C     '* DELETE'    CAT       'D'           XXDEL             9
     C                   MOVEL     XXDEL         SCDEL
     C                   MOVE      '1'           *IN08
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    SCR01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ELSE
     C                   MOVE      *BLANKS       SCCPT4
     C                   Z-ADD     *ZEROS        SCAAMT
     C                   MOVE      *BLANKS       SCTYP
     C                   MOVE      *BLANKS       SCDEL
     C     '* DELETE'    CAT       'D'           XXDEL             9
     C                   MOVEL     XXDEL         SCDEL
     C                   MOVE      '1'           *IN08
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    SCR01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   END
     C                   ELSE
     C     *IN25         IFEQ      '0'
     C                   UPDATE    SCR01S
     C                   END
     C                   END
     C*
     C                   GOTO      NXTDC4
     C                   END                                                    END *IN75
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
     C                   MOVE      '1'           *IN24
     C                   MOVE      '1'           *IN26
     C                   GOTO      MAINLN
     C                   END
     C*
     C                   MOVE      RRN2          SRRN2             5 0
     C                   Z-ADD     *ZEROS        K                 5 0
     C     NXTDC2        TAG
     C                   ADD       1             K
     C     K             CHAIN     SCR01S                             75
     C     *IN75         IFEQ      '0'
     C*
     C     SCCPT4        IFEQ      *BLANKS
     C     SCAAMT        ANDEQ     *ZEROS
     C     SCTYP         ANDEQ     *BLANKS
     C                   GOTO      NXTDC2
     C                   ELSE
     C*
     C* IF ANYTHING ON LINE ENTERED CPT4 CODE MUST BE ENTERED
     C*
     C     SCCPT4        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN22
     C                   UPDATE    SCR01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ELSE
     C                   MOVE      '0'           *IN22
     C                   END                                                    END SCCPT4
     C*
     C* THE TYPE CODE FLAG MUST BE A 'C' OR 'I'
     C*
     C     SCTYP         IFNE      'C'
     C     SCTYP         ANDNE     'I'
     C                   MOVE      '1'           *IN28
     C                   UPDATE    SCR01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ELSE
     C                   MOVE      '0'           *IN28
     C                   END
     C*
     C* THE CPT4 CODE MUST BE VALID CODE
     C*
     C     SCTYP         IFEQ      'C'
     C                   MOVEL     SCCPT4        XXCPT4
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  60
     C     *IN60         IFEQ      '0'
     C     PRCO#         ANDEQ     UPCO#
     C     PRCPT4        ANDEQ     XXCPT4
     C                   MOVE      '0'           *IN23
     C                   ELSE
     C                   MOVE      '1'           *IN23
     C                   UPDATE    SCR01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   END                                                    END *IN60
     C                   ENDIF                                                  TYP = 'C'
     C     SCTYP         IFEQ      'I'
     C                   MOVEL     SCCPT4        XXICD9
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '0'           *IN23
     C                   ELSE
     C                   MOVE      '1'           *IN23
     C                   UPDATE    SCR01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   END                                                    END *IN60
     C                   ENDIF                                                  TYP = 'I'
     C*
     C     *IN22         IFEQ      '0'
     C     *IN23         ANDEQ     '0'
     C                   UPDATE    SCR01S
     C                   END
     C*
     C                   GOTO      NXTDC2
     C                   END                                                    END SCCPT4
     C                   END                                                    END *IN75
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
     C                   WRITE     SCR01C
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
     C                   DO        9             X                 5 0
     C*
     C     COKY2         READE     PCO02                                  60
     C*
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    PCO02E
     C*
     C                   ADD       1             RRN2              5 0
     C                   MOVEL     COCPT4        SCCPT4
     C                   Z-ADD     COAAMT        SCAAMT
     C                   MOVEL     COFL01        SCTYP
     C*
     C*   Save 1st subfile RRN of the current page
     C*
     C     PCFL          IFEQ      'Y'
     C                   Z-ADD     RRN2          XXRRN2            5 0
     C                   MOVE      'N'           PCFL
     C                   END
     C                   WRITE     SCR01S
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       SCSEL
     C                   MOVE      *BLANKS       SCCPT4
     C                   Z-ADD     *ZEROS        SCAAMT
     C                   MOVE      *BLANKS       SCTYP
     C                   ADD       1             RRN2
     C*
     C*   Save 1st subfile RRN of the current page
     C*
     C     PCFL          IFEQ      'Y'
     C                   Z-ADD     RRN2          XXRRN2
     C                   MOVE      'N'           PCFL
     C                   END
     C                   WRITE     SCR01S
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
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
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
     C     BDKY1         CHAIN     PBD01                              41
     C*
     C                   Z-ADD     *ZEROS        K                 5 0
     C     NXTDC3        TAG
     C                   ADD       1             K
     C     K             CHAIN     SCR01S                             75
     C     *IN75         IFEQ      '0'
     C*
     C     SCCPT4        IFEQ      *BLANKS
     C     SCAAMT        ANDEQ     *ZEROS
     C     SCTYP         ANDEQ     *BLANKS
     C                   GOTO      NXTDC3
     C                   ELSE
     C*
     C     COKY1         CHAIN     PCO02                              60
     C*
     C                   MOVEL     SCCODE        COCODE
     C                   Z-ADD     YMEFDT        COEFDT
     C                   MOVEL     SCCPT4        COCPT4
     C                   Z-ADD     0             COFEE
     C                   Z-ADD     SCAAMT        COAAMT
     C                   MOVE      SCTYP         COFL01
     C                   MOVE      *BLANKS       COFL02
     C                   MOVE      *BLANKS       COFL03
     C                   MOVE      *BLANKS       COFL04
     C                   MOVE      *BLANKS       COFL05
     C                   MOVE      *BLANKS       COFL06
     C                   MOVE      *BLANKS       COFL07
     C                   MOVE      *BLANKS       COFL08
     C                   MOVE      *BLANKS       COFL09
     C                   MOVE      *BLANKS       COFL10
     C                   MOVE      *BLANKS       CO10A1
     C                   MOVE      *BLANKS       CO10A2
     C                   MOVE      *BLANKS       CO10A3
     C                   MOVE      *BLANKS       CO10A4
     C                   MOVE      *BLANKS       CO10A5
     C                   Z-ADD     *ZEROS        CO6N1
     C                   Z-ADD     *ZEROS        CO6N2
     C                   Z-ADD     *ZEROS        CO6N3
     C                   Z-ADD     *ZEROS        CO6N4
     C                   Z-ADD     *ZEROS        CO6N5
     C                   MOVE      SCSTAT        COSTAT
     C                   MOVEL     Q1USER        COUSER
     C                   MOVEL     Q1PGM         COPGM
     C                   Z-ADD     DSTIME        COTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CODT
     C*
     C     *IN60         IFEQ      '0'
     C                   UPDATE    PCO02
     C                   ELSE
     C                   MOVEL     Q1USER        COIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         COIDT
     C*
     C                   WRITE     PCO02
     C                   END
     C                   GOTO      NXTDC3
     C*
     C                   END                                                    END SCCPT4
     C                   END                                                    END *IN75
     C*
     C*  ALL EDITS HAVE BEEN PASSED  *** WRITE/UPDATE FILE!
     C*
     C                   MOVEL     SCCODE        BDCODE
     C                   MOVE      YMEFDT        BDEFDT
     C                   MOVE      SCSTAT        BDSTAT
     C                   MOVE      SCCOM1        BDCOM1
     C                   MOVE      SCCOM2        BDCOM2
     C                   Z-ADD     *ZEROS        BDPC01
     C                   Z-ADD     *ZEROS        BDPC02
     C                   Z-ADD     *ZEROS        BDPC03
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
     C*
     C     *IN41         IFEQ      '0'
     C                   UPDATE    PBD01
     C                   ELSE
     C                   MOVE      Q1USER        BDIUSR
     C                   WRITE     PBD01
     C                   END
     C*
     C                   ENDSR
     C*
     C/EJECT
     C*===============================================================
     C     FKEYS         BEGSR
     C*===============================================================
     C*
     C     *INKA         IFEQ      '1'
     C                   END
     C*
     C     *INKB         IFEQ      '1'
     C                   END
     C*
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C*
     C     *INKD         IFEQ      '1'
     C                   END
     C*
     C****************************************
     C*   F5 = REFRESH                       *
     C****************************************
     C     *INKE         IFEQ      '1'
     C*
     C*   Clear subfile
     C*
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C                   Z-ADD     0             XXRRN2
     C*
     C     COKY          SETLL     PCO02                              60
     C*
     C                   EXSR      LOAD
     C*
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      MAINLN
     C                   END
     C*
     C****************************************
     C*   PROCEDURE CODE SEARCH              *
     C****************************************
     C*** BEGIN CHG *** 09/20/00 **************************************
     C********** *INKF     IFEQ '1'
     C********** HFLD      ANDEQ'SCCPT4'
     C**********           MOVELSCCPT4    QQCPT4  5 P
     C**********           MOVE *ZEROS    QQFDAT  80
     C**********           CALL 'F0520R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQCPT4
     C**********           PARM           QQFDAT
     C**********           PARM 'UC'      QQST    2
     C**********           PARM           QQRTCD  1
     C********** QQRTCD    CABEQ'A'       TERM
     C********** QQRTCD    IFEQ '*'
     C********** SFRRN     CHAINSCR01S               75
     C**********           MOVELQQCPT4    SCCPT4    P
     C**********           MOVEL'C'       SCTYP
     C**********           SETON                     35
     C**N75*****           UPDATSCR01S
     C**********           ENDIF                           RTCD = '*'
     C**********           ENDIF
     C     *INKJ         IFEQ      *ON
     C     HFLD          ANDEQ     'SCCPT4'
     C*
     C                   CALL      'F2015R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##PROC            5
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     *INKL         IFEQ      *ON
     C     ##PROC        OREQ      *BLANKS
     C*
     C                   SETOFF                                       KL
     C     SFRRN         CHAIN     SCR01S                             75
     C                   SETON                                        36
     C  N75              UPDATE    SCR01S
     C*
     C                   ELSE
     C*
     C     SFRRN         CHAIN     SCR01S                             75
     C                   MOVEL(P)  ##PROC        SCCPT4
     C                   MOVEL     ##RTCD        SCTYP
     C                   SETON                                        35
     C  N75              UPDATE    SCR01S
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   CHG *** 09/20/00 **************************************
     C*
     C****************************************
     C*   ICD-9 CODE SEARCH                  *
     C****************************************
     C*** BEGIN DEL *** 09/20/00 **************************************
     C********** *INKG     IFEQ '1'
     C**********           MOVELSCCPT4    QQICD9  5 P
     C**********           CALL 'F0510R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQICD9
     C**********           PARM           QQRTCD  1
     C********** QQRTCD    CABEQ'A'       TERM
     C********** QQRTCD    IFEQ '*'
     C********** SFRRN     CHAINSCR01S               75
     C**********           MOVELQQICD9    SCCPT4    P
     C**********           MOVEL'I'       SCTYP
     C**N75*****           UPDATSCR01S
     C**********           ENDIF                           RTCD = '*'
     C**********           ENDIF                           *INKG = On
     C*** END   DEL *** 09/20/00 **************************************
     C*
     C     *INKH         IFEQ      '1'
     C                   END
     C*
     C     *INKI         IFEQ      '1'
     C                   END
     C*
     C     *INKJ         IFEQ      '1'
     C                   END
     C*
     C     *INKK         IFEQ      '1'
     C                   END
     C*
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01S'
     C     Q2RECF        OREQ      'SCR01C'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C                   END                                                    END *INKL
     C*
     C     *INKM         IFEQ      '1'
     C                   END
     C*
     C     *INKN         IFEQ      '1'
     C                   END
     C*
     C     *INKP         IFEQ      '1'
     C                   END
     C*
     C     *INKQ         IFEQ      '1'
     C                   END
     C*
     C     *INKR         IFEQ      '1'
     C                   END
     C*
     C     *INKS         IFEQ      '1'
     C                   END
     C*
     C     *INKT         IFEQ      '1'
     C                   END
     C*
     C****************************************
     C*   F20 = CHANGE STATUS                *
     C****************************************
     C     *INKU         IFEQ      '1'
     C     *IN40         ANDEQ     '0'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   MOVE      '1'           *IN64                           ALLOW DELETE
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      '0'           *IN64                           NO DELETING
     C                   END
     C                   GOTO      MAINLN
     C                   END
     C*
     C     *INKV         IFEQ      '1'
     C                   END
      *
     C     *INKW         IFEQ      '1'
     C                   END
     C*
     C****************************************
     C*   F23 = DELETE CODE                  *
     C****************************************
     C     *INKX         IFEQ      '1'
     C     *IN64         ANDEQ     '1'
     C                   DELETE    PBD01
     C     COKY          CHAIN     PCO02                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PCO02
     C     COKY          READE     PCO02                                  60
     C                   END
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           MAINLN                         NO ROLL IF EOF
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
NETWORK DISC: CARVE OUT PER CODE
