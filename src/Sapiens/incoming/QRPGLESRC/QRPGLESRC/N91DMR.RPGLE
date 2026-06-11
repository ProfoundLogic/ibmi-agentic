     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE N91DMR - Type DM Discount Code Maintenance
     H*****************************************************************
     H*  Program Description:                                         *
     H*    Allows entry of specific DM Discount Code Information      *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    N0920R                                                     *
     H*    N2330R                                                     *
     H*    N2330R01                                                   *
     H*                                                               *
     H*****************************************************************
     H*  Created on 04/09/2007 by c:62 for Project 25MOD              *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*
     H*  04/05/18 C:079 Support 25 69974
     H*                 Remove hard edit to allow entry of 0.00
     H*
     H*  07/22/14 C:079 SUPPORT 85633
     H*                 Relax edit on UC by removing the date
     H*
     H*  06/23/14 C:296 SUPPORT 85633
     H*                 1) Perform proper update of codes.
     H*                 2) Correct logic for error message, overlapping codes.
     H*
     H*  12/04/09 C:198 SUPPORT 70760
     H*                 CORRECTED MODE INFO WHEN IN DISPLAY MODE.
     H*
     H*  09/08/08 C:62 PROJECT 25MPA
     H*                ADD MODIFIER LOGIC
     H*
     H*****************************************************************
     FN91DMF    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(WDW01S:RRN2)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FNMBDLD    UF A E           K DISK
     F*** Beg Chg *** 06/23/14 ****************************************
     F*** BEGIN CHG *** 09/08/08 **************************************
     F*****NDCOL3  UF  E           K        DISK                      A
     F*****NDCOL6    UF A E           K DISK
     FNDCOL6    UF A E           K DISK    INFDS(MYDDS)
     F*** END   CHG *** 09/08/08 **************************************
     F*** End Chg *** 06/23/14 ****************************************
     F                                     RENAME(PCO01:PCO02)
     FFMPRLUC   IF   E           K DISK
     F*** BEGIN ADD *** 09/08/08 **************************************
     FFTPMP     IF   E             DISK    USROPN
     FFTABL     IF   E           K DISK    USROPN
     F                                     RENAME(PPM01:PPM02)

     D*** BEGIN ADD *** 09/08/08 **************************************
     D QCA             S             56    DIM(4) CTDATA PERRCD(1)
     D*** END   ADD *** 09/08/08 **************************************
     F*** END   ADD *** 09/08/08 **************************************
     D*** BEG CHG *** 12/04/09 ****************************************
     D*******             UA      1   2 10
     D UA              S             10    DIM(3) CTDATA PERRCD(1)
     D*** END CHG *** 12/04/09 ****************************************
     D TD              S             70    DIM(1) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*** BEGIN ADD *** 09/08/08 **************************************
     D                 DS
     D  QQCMD                  1     56
     D  QQST                  40     41
     D*** END   ADD *** 09/08/08 **************************************
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  CURSOR               370    371B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** Beg Chg *** 06/23/14 ****************************************
     DMYDDS            DS
     D rrnNDCOL6             397    400B 0
     D*
     Dflgupd           S              1A   inz('0')
     DrrnLOop          S              5  0 inz(1)
     Dintdelflg        S              1A   inz('0')
     D arRN            S              5  0 DIM(999)
     Dintcnt           S              5  0 inz(0)
     D*** End Chg *** 06/23/14 ****************************************
     c*** Beg Add *** 04/05/18 ****************************************
     c/copy sPrGenPop
     c*** End Add *** 04/05/18 ***************************************

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
     C*** BEGIN ADD *** 09/08/08 **************************************
     C                   KFLD                    KKPMOD
     C                   KFLD                    KKPMD2
     C                   KFLD                    KKPMD3
     C*** END   ADD *** 09/08/08 **************************************
     C*
     C     COKY1         KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKEFDT
     C                   KFLD                    KK10A1
     C*
     C     COKY2         KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKEFDT
     C*** BEGIN ADD *** 09/08/08 **************************************
     C*
     C     PMKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    KKKEY
     C*** END   ADD *** 09/08/08 **************************************
     C*
     C     PRKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXCPT4            5
     C                   KFLD                    KKEFDT
     C*
     C     PRKY2         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXCPT4
     C*
     C     *LIKE         DEFINE    UPDT          KKEFDT
     C     *LIKE         DEFINE    CO10A1        KK10A1
     C     *LIKE         DEFINE    CO10A1        KK10A2
     C     *LIKE         DEFINE    COCODE        KKCODE
     C*** BEGIN ADD *** 09/08/08 **************************************
     C     *LIKE         DEFINE    PMKEY         KKKEY
     C     *LIKE         DEFINE    COPMOD        KKPMOD
     C     *LIKE         DEFINE    COPMD2        KKPMD2
     C     *LIKE         DEFINE    COPMD3        KKPMD3
     C*** END   ADD *** 09/08/08 **************************************
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
     C*** BEGIN ADD *** 09/08/08 **************************************
     C                   MOVEL(P)  QCA(1)        QQCMD
     C                   MOVEL     'TX'          QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM      56            LENGTH           15 5
     C*
     C                   MOVEL(P)  QCA(2)        QQCMD
     C                   MOVEL     'TX'          QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM      56            LENGTH           15 5
     C*
     C                   OPEN      FTABL
     C                   OPEN      FTPMP
     C*
     C     1             CHAIN     PPM01                              99
     C     777           CHAIN     PPM02                              99
     C*** END   ADD *** 09/08/08 **************************************
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
     C                   MOVE      'DM'          SCTYPE
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
     C*** BEG ADD *** 12/04/09 *******************************************
     C     *IN09         IFEQ      *OFF
     C*** END ADD *** 12/04/09 *******************************************
     C                   MOVE      UA(2)         SCMODE                         RECORD
     C*** BEG ADD *** 12/04/09 *******************************************
     C                   ELSE
     C                   MOVE      UA(3)         SCMODE                         RECORD
     C                   ENDIF                                                  DISPLAY
     C*** END ADD *** 12/04/09 *******************************************
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
     C***** Beg Chg ***** 06/23/14 ******************************
     C*****     *IN11         CASEQ     '1'           FKEYS
     C*****                   END                                                      END *IN11
     C                   Exsr      FKEYS
     C***** End Chg ***** 06/23/14 ******************************
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
     C***** Beg Chg ***** 06/23/14 ******************************
     C******IN50         IFEQ      '0'
      /Free
         intcnt = 0;
         DoW not %Eof(N91DMF) ;
      /End-Free
     C***** End Chg ***** 06/23/14 ******************************
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** SAVE THAT DATA TO BE PASSED BACK TO THE CALLING PROGRAM
     C     SCSEL         IFEQ      '4'
     C***** Beg Chg ***** 06/23/14 ******************************
     C*****              EXSR      DELDSC
      /Free
         intdelflg = '1';
      /End-Free
     C***** End Chg ***** 06/23/14 ******************************
     C                   ELSE                                                   ELSE EQ '1'
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '4' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   Z-ADD     RRN2          #@$2
     C                   UPDATE    WDW01S
     C                   MOVE      *BLANK        SCSEL
     C                   GOTO      MAINLN
     C***** Beg Chg ***** 06/23/14 ******************************
      /Free
        Else;                                                           //When SCSEL = *Blanks
          intcnt += 1;
          arRN(intcnt) = RRN2;
      /End-Free
     C***** End Chg ***** 06/23/14 ******************************
     C                   END                                                    END NE ' '
     C***** Beg Chg ***** 06/23/14 ******************************
     C*****              EXSR      DELDSC
      /Free
         if intdelflg = '1';
            exsr DELDSC;
         Endif;
      /End-Free
     C***** End Chg ***** 06/23/14 ******************************
     C                   END                                                    END EQ '4'
     C***** Beg Chg ***** 06/23/14 ******************************
     C*****              END                                                    END 0 *IN50
      /Free
         ReadC WDW01S;
         Enddo;
      /End-Free
     C***** Beg Chg ***** 06/23/14 ******************************
     C*
     C  N09              EXSR      EDITDC
     C                   EXSR      WRITDC
     C*
     C*===============================================================
     C* TERMINATION OF PROGRAM
     C*
     C     TERM          TAG
     C*** BEGIN ADD *** 09/08/08 **************************************
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*
     C                   CLOSE     FTABL
     C                   CLOSE     FTPMP
     C*** END   ADD *** 09/08/08 **************************************
     C*
     C                   MOVE      '1'           *INLR
     C*
     C/EJECT
     C*====================================================================*
     C* * *   CHKMOD SUBROUTINE                              C H K M O D ***
     C*====================================================================*
     C     CHKMOD        BEGSR
     C*
     C                   MOVE      *OFF          ERROR             1
     C                   MOVE      *OFF          HITMD1            1
     C                   MOVE      *OFF          HITMD2            1
     C                   MOVE      *OFF          HITMD3            1
     C                   MOVE      *OFF          HITMD4            1
     C                   MOVE      *OFF          HITMD5            1
     C                   MOVE      *OFF          HITMD6            1
     C                   MOVE      *OFF          OVRLAP            1
     C*
     C     SC10A1        IFGE      CO10A1
     C     SC10A1        ANDLE     CO10A2
     C     SC10A2        ORGE      CO10A1
     C     SC10A2        ANDLE     CO10A2
     C     SC10A1        ORLT      CO10A1
     C     SC10A2        ANDGE     CO10A1
     C     SC10A2        ORGT      CO10A2
     C     SC10A1        ANDLE     CO10A2
     C                   MOVE      *ON           OVRLAP
     C                   ENDIF
     C*
     C     OVRLAP        IFEQ      *ON
     C*
     C     SCPMOD        IFEQ      *BLANKS
     C     SCPMD2        ANDEQ     *BLANKS
     C     SCPMD3        ANDEQ     *BLANKS
     C     COPMOD        IFEQ      *BLANKS
     C     COPMD2        ANDEQ     *BLANKS
     C     COPMD3        ANDEQ     *BLANKS
     C                   MOVE      *ON           HITMD1
     C                   MOVE      *ON           HITMD2
     C                   MOVE      *ON           HITMD3
     C                   MOVE      *ON           HITMD4
     C                   MOVE      *ON           HITMD5
     C                   MOVE      *ON           HITMD6
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCPMOD        IFNE      *BLANKS
     C     SCPMOD        IFEQ      COPMOD
     C     SCPMOD        OREQ      COPMD2
     C     SCPMOD        OREQ      COPMD3
     C                   MOVE      *ON           HITMD1
     C                   ENDIF
     C                   ELSE
     C     COPMOD        IFEQ      *BLANKS
     C     COPMD2        OREQ      *BLANKS
     C     COPMD3        OREQ      *BLANKS
     C                   MOVE      *ON           HITMD1
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCPMD2        IFNE      *BLANKS
     C     SCPMD2        IFEQ      COPMOD
     C     SCPMD2        OREQ      COPMD2
     C     SCPMD2        OREQ      COPMD3
     C                   MOVE      *ON           HITMD2
     C                   ENDIF
     C                   ELSE
     C     COPMOD        IFEQ      *BLANKS
     C     COPMD2        OREQ      *BLANKS
     C     COPMD3        OREQ      *BLANKS
     C                   MOVE      *ON           HITMD2
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCPMD3        IFNE      *BLANKS
     C     SCPMD3        IFEQ      COPMOD
     C     SCPMD3        OREQ      COPMD2
     C     SCPMD3        OREQ      COPMD3
     C                   MOVE      *ON           HITMD3
     C                   ENDIF
     C                   ELSE
     C     COPMOD        IFEQ      *BLANKS
     C     COPMD2        OREQ      *BLANKS
     C     COPMD3        OREQ      *BLANKS
     C                   MOVE      *ON           HITMD3
     C                   ENDIF
     C                   ENDIF
     C*
     C     COPMOD        IFNE      *BLANKS
     C     COPMOD        IFEQ      SCPMOD
     C     COPMOD        OREQ      SCPMD2
     C     COPMOD        OREQ      SCPMD3
     C                   MOVE      *ON           HITMD4
     C                   ENDIF
     C                   ELSE
     C     SCPMOD        IFEQ      *BLANKS
     C     SCPMD2        OREQ      *BLANKS
     C     SCPMD3        OREQ      *BLANKS
     C                   MOVE      *ON           HITMD4
     C                   ENDIF
     C                   ENDIF
     C*
     C     COPMD2        IFNE      *BLANKS
     C     COPMD2        IFEQ      SCPMOD
     C     COPMD2        OREQ      SCPMD2
     C     COPMD2        OREQ      SCPMD3
     C                   MOVE      *ON           HITMD5
     C                   ENDIF
     C                   ELSE
     C     SCPMOD        IFEQ      *BLANKS
     C     SCPMD2        OREQ      *BLANKS
     C     SCPMD3        OREQ      *BLANKS
     C                   MOVE      *ON           HITMD5
     C                   ENDIF
     C                   ENDIF
     C*
     C     COPMD3        IFNE      *BLANKS
     C     COPMD3        IFEQ      SCPMOD
     C     COPMD3        OREQ      SCPMD2
     C     COPMD3        OREQ      SCPMD3
     C                   MOVE      *ON           HITMD6
     C                   ENDIF
     C                   ELSE
     C     SCPMOD        IFEQ      *BLANKS
     C     SCPMD2        OREQ      *BLANKS
     C     SCPMD3        OREQ      *BLANKS
     C                   MOVE      *ON           HITMD6
     C                   ENDIF
     C                   ENDIF
     C*
     C     HITMD1        IFEQ      *ON
     C     HITMD2        ANDEQ     *ON
     C     HITMD2        ANDEQ     *ON
     C     HITMD4        ANDEQ     *ON
     C     HITMD5        ANDEQ     *ON
     C     HITMD6        ANDEQ     *ON
     C                   MOVE      *ON           ERROR
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
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
     C*** BEGIN ADD *** 09/08/08 **************************************
     C                   MOVEL(P)  SCPMOD        KKPMOD
     C                   MOVEL(P)  SCPMD2        KKPMD2
     C                   MOVEL(P)  SCPMD3        KKPMD3
     C*** END   ADD *** 09/08/08 **************************************
     C     COKY          CHAIN     PCO02                              60
     C     *IN60         IFEQ      '0'
     C                   DELETE    PCO02
     C                   MOVE      *BLANKS       SC10A1
     C                   MOVE      *BLANKS       SC10A2
     C*** BEGIN ADD *** 09/08/08 **************************************
     C                   MOVE      *BLANKS       SCPMOD
     C                   MOVE      *BLANKS       SCPMD2
     C                   MOVE      *BLANKS       SCPMD3
     C                   Z-ADD     *ZEROS        SCRATE
     C*** END   ADD *** 09/08/08 **************************************
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
     C*** BEGIN ADD *** 09/08/08 **************************************
     C                   MOVE      *BLANKS       SCPMOD
     C                   MOVE      *BLANKS       SCPMD2
     C                   MOVE      *BLANKS       SCPMD3
     C                   Z-ADD     *ZEROS        SCRATE
     C*** END   ADD *** 09/08/08 **************************************
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
     C                   MOVE      '1'           *IN75
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
     C*** BEGIN ADD *** 09/08/08 **************************************
     C     SCPMOD        ANDEQ     *BLANKS
     C     SCPMD2        ANDEQ     *BLANKS
     C     SCPMD3        ANDEQ     *BLANKS
     C     SCRATE        ANDEQ     *ZEROS
     C*** END   ADD *** 09/08/08 **************************************
     C                   GOTO      NXTDC2
     C                   ELSE
     C*
     C* IF ANYTHING ON LINE ENTERED FROM CODE MUST BE ENTERED
     C*
     C     SC10A1        IFNE      *BLANKS
     C                   MOVEL     SC10A1        XXCPT4
     C*** Beg Chg *** 07/22/14 *********************************************
     C**** PRKY1         SETGT     PPR01
     C**** PRKY2         READPE    PPR01                                  99
     C     PRKY2         CHAIN     PPR01                              99
     C*** End Chg *** 07/22/14 *********************************************
     C     *IN99         IFEQ      *OFF
     C                   MOVE      '0'           *IN71
     C                   ELSE
     C                   MOVE      '1'           *IN71
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ENDIF                                                  END *IN99
     C                   ENDIF
     C*
     C* IF ANYTHING ON LINE ENTERED TO CODE MUST BE ENTERED
     C*
     C     SC10A2        IFNE      *BLANKS
     C     SC10A1        ORNE      *BLANKS
     C                   MOVEL     SC10A2        XXCPT4
     C*** Beg Chg *** 07/22/14 *********************************************
     C**** PRKY1         SETGT     PPR01
     C**** PRKY2         READPE    PPR01                                  99
     C     PRKY2         CHAIN     PPR01                              99
     C*** End Chg *** 07/22/14 *********************************************
     C     *IN99         IFEQ      *OFF
     C                   MOVE      '0'           *IN72
     C                   ELSE
     C                   MOVE      '1'           *IN72
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ENDIF                                                  END *IN99
     C                   ENDIF
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
     C*** BEGIN ADD *** 09/08/08 **************************************
     C*
     C* IF MODIFIER 1 IS ENTERED IT MUST BE VALID
     C*
     C     SCPMOD        IFNE      *BLANKS
     C                   MOVEL(P)  SCPMOD        KKKEY
     C     PMKY          CHAIN     PPM02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      '0'           *IN76
     C                   ELSE
     C                   MOVE      '1'           *IN76
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ENDIF                                                  END *IN99
     C                   ENDIF
     C*
     C* IF MODIFIER 2 IS ENTERED IT MUST BE VALID
     C*
     C     SCPMD2        IFNE      *BLANKS
     C                   MOVEL(P)  SCPMD2        KKKEY
     C     PMKY          CHAIN     PPM02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      '0'           *IN77
     C                   ELSE
     C                   MOVE      '1'           *IN77
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ENDIF                                                  END *IN99
     C                   ENDIF
     C*
     C* IF MODIFIER 3 IS ENTERED IT MUST BE VALID
     C*
     C     SCPMD3        IFNE      *BLANKS
     C                   MOVEL(P)  SCPMD3        KKKEY
     C     PMKY          CHAIN     PPM02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      '0'           *IN78
     C                   ELSE
     C                   MOVE      '1'           *IN78
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ENDIF                                                  END *IN99
     C                   ENDIF
     C*
     C* DON'T ALLOW OVERLAPS, SO LET'S CHECK IF THE LINE IS BEING ADDED
     C***** Beg Chg ***** 06/23/14 ******************************
      /Free
           rrnLoop = 1;
      /End-free
     C***** End Chg ***** 06/23/14 ******************************
     C*
     C                   MOVEL(P)  SC10A1        KK10A1
     C                   MOVEL(P)  SC10A2        KK10A2
     C                   MOVEL(P)  SCPMOD        KKPMOD
     C                   MOVEL(P)  SCPMD2        KKPMD2
     C                   MOVEL(P)  SCPMD3        KKPMD3
     C     COKY          CHAIN(N)  PCO02                              99
     C     *IN99         IFEQ      *ON
     C     COKY2         CHAIN     PCO02                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C***** Beg Chg ***** 06/23/14 ******************************
      /Free
           ERROR = *Off;
           If %lookup(rrnLoop:arRN) > 0;
      /End-Free
     C     COKY2         READE     PCO02                                  99
      /Free
             rrnLoop += 1;
             Iter;
           Endif;
      /End-free
     C***** End Chg ***** 06/23/14 ******************************
     C                   EXSR      CHKMOD
     C     ERROR         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C     COKY2         READE     PCO02                                  99
     C***** Beg Chg ***** 06/23/14 ******************************
      /Free
           rrnLoop += 1;
      /End-free
     C***** End Chg ***** 06/23/14 ******************************
     C                   ENDDO
     C*
     C     ERROR         IFEQ      *ON
     C                   MOVE      '1'           *IN80
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ELSE
     C                   MOVE      '0'           *IN80
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 09/08/08 **************************************
     C*
     C* AMOUNT CAN'T BE ZERO
     C*
     C     SCFEE         IFEQ      *ZEROS
     C*** BEGIN ADD *** 09/08/08 **************************************
     C     SCRATE        ANDEQ     *ZEROS
     C*** END   ADD *** 09/08/08 **************************************
     C*** Beg Del *** 04/05/18 ****************************************
     C***************    MOVE      '1'           *IN74
     C*** End Del *** 04/05/18 ****************************************
     C*
     C*** Beg Add *** 04/05/18 ****************************************
      /Free

           PopOneBtn('% CMS and Flat Rate are both zero on one of the +
            records.  This range of codes will pay zero.':' ');

      /End-Free
     C*
     C*** End Add *** 04/05/18 ****************************************
     C*** Beg Del *** 04/05/18 ****************************************
     C**************
     C**************     UPDATE    WDW01S
     C**************     MOVE      SRRN2         RRN2
     C**************     Z-ADD     K             #@$2
     C**************     GOTO      MAINLN
     C**************     ELSE
     C**************     MOVE      '0'           *IN74
     C*** End Del *** 04/05/18 ****************************************
     C                   ENDIF
     C*** BEGIN ADD *** 09/08/08 **************************************
     C*
     C* ONLY AMOUNT OR FEE CAN BE ENTERED
     C*
     C     SCFEE         IFNE      *ZEROS
     C     SCRATE        ANDNE     *ZEROS
     C                   MOVE      '1'           *IN79
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ELSE
     C                   MOVE      '0'           *IN79
     C                   ENDIF
     C*** END   ADD *** 09/08/08 **************************************
     C*
     C     *IN71         IFEQ      '0'
     C     *IN72         ANDEQ     '0'
     C     *IN73         ANDEQ     '0'
     C     *IN74         ANDEQ     '0'
     C     *IN75         ANDEQ     '0'
     C*** BEGIN ADD *** 09/08/08 **************************************
     C     *IN76         ANDEQ     '0'
     C     *IN77         ANDEQ     '0'
     C     *IN78         ANDEQ     '0'
     C     *IN79         ANDEQ     '0'
     C*** END   ADD *** 09/08/08 **************************************
     C                   UPDATE    WDW01S
     C                   ENDIF
     C*
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
     C*** BEGIN ADD *** 09/08/08 **************************************
     C                   MOVEL(P)  COPMOD        SCPMOD
     C                   MOVEL(P)  COPMD2        SCPMD2
     C                   MOVEL(P)  COPMD3        SCPMD3
     C                   Z-ADD     CORATE        SCRATE
     C*** END   ADD *** 09/08/08 **************************************
     C                   Z-ADD     COFEE         SCFEE
     C                   MOVEL     *BLANKS       SCDEL
     C***** Beg Add ***** 06/23/14 ************************************
     C* Moving hidden fields -
     C                   MOVEL(P)  CO10A1        SCH10A1
     C                   MOVEL(P)  CO10A2        SCH10A2
     C                   MOVEL(P)  COPMOD        SCHPMOD
     C                   MOVEL(P)  COPMD2        SCHPMD2
     C                   MOVEL(P)  COPMD3        SCHPMD3
     C                   Z-ADD     CORATE        SCHRATE
     C                   Z-ADD     COFEE         SCHFEE
     C                   z-Add     RRN2          SCHRRN2
     C***** End Add ***** 06/23/14 ************************************
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
     C*** BEGIN ADD *** 09/08/08 **************************************
     C                   MOVEL(P)  *BLANKS       SCPMOD
     C                   MOVEL(P)  *BLANKS       SCPMD2
     C                   MOVEL(P)  *BLANKS       SCPMD3
     C                   Z-ADD     *ZEROS        SCRATE
     C*** END   ADD *** 09/08/08 **************************************
     C***** Beg Add ***** 06/23/14 ************************************
     C* Moving hidden fields -
     C                   MOVEL(P)  *BLANKS       SCH10A1
     C                   MOVEL(P)  *BLANKS       SCH10A2
     C                   MOVEL(P)  *BLANKS       SCHPMOD
     C                   MOVEL(P)  *BLANKS       SCHPMD2
     C                   MOVEL(P)  *BLANKS       SCHPMD3
     C                   Z-ADD     *ZEROS        SCHRATE
     C                   Z-ADD     *ZEROS        SCHFEE
     C                   z-Add     RRN2          SCHRRN2
     C***** End Add ***** 06/23/14 ************************************
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
     C*** BEGIN ADD *** 09/08/08 **************************************
     C     SCPMOD        ANDEQ     *BLANKS
     C     SCPMD2        ANDEQ     *BLANKS
     C     SCPMD3        ANDEQ     *BLANKS
     C     SCRATE        ANDEQ     *ZEROS
     C*** END   ADD *** 09/08/08 **************************************
     C                   GOTO      NXTDC3
     C                   ELSE
     C*
     C***** Beg Chg ***** 06/23/14 ******************************
      /Free
        If SCH10A1 = *blanks and SCH10A2 = *blanks and SCHPMOD = *blanks
           and SCHPMD2 = *blanks and SCHPMD3 = *blanks;
      /End-Free
     C***** End Chg ***** 06/23/14 ******************************
     C                   MOVEL(P)  SC10A1        KK10A1
     C                   MOVEL(P)  SC10A2        KK10A2
     C*** BEGIN ADD *** 09/08/08 **************************************
     C                   MOVEL(P)  SCPMOD        KKPMOD
     C                   MOVEL(P)  SCPMD2        KKPMD2
     C                   MOVEL(P)  SCPMD3        KKPMD3
     C*** END   ADD *** 09/08/08 **************************************
     C***** Beg Chg ***** 06/23/14 ******************************
      /Free
        Else;
           KK10A1 = SCH10A1 ;
           KK10A2 = SCH10A2 ;
           KKPMOD = SCHPMOD ;
           KKPMD2 = SCHPMD2 ;
           KKPMD3 = SCHPMD3 ;
        Endif;
      /End-Free
     C***** End Chg ***** 06/23/14 ******************************
     C     COKY          CHAIN     PCO02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     SCCODE        COCODE
     C                   Z-ADD     KKEFDT        COEFDT
     C                   MOVEL(P)  SC10A1        CO10A1
     C                   MOVEL(P)  SC10A2        CO10A2
     C*** BEGIN ADD *** 09/08/08 **************************************
     C                   MOVEL(P)  SCPMOD        COPMOD
     C                   MOVEL(P)  SCPMD2        COPMD2
     C                   MOVEL(P)  SCPMD3        COPMD3
     C                   Z-ADD     SCRATE        CORATE
     C*** END   ADD *** 09/08/08 **************************************
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
     C                   ELSE
     C                   CLEAR                   PCO02
     C                   MOVEL     SCCODE        COCODE
     C                   Z-ADD     KKEFDT        COEFDT
     C                   MOVEL(P)  SC10A1        CO10A1
     C                   MOVEL(P)  SC10A2        CO10A2
     C*** BEGIN ADD *** 09/08/08 **************************************
     C                   MOVEL(P)  SCPMOD        COPMOD
     C                   MOVEL(P)  SCPMD2        COPMD2
     C                   MOVEL(P)  SCPMD3        COPMD3
     C                   Z-ADD     SCRATE        CORATE
     C*** END   ADD *** 09/08/08 **************************************
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
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SC10A1'
     C                   CALL      'F2015R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##PROC            5
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      '1'           ##DFT             1
     C*
     C     *INKL         IFEQ      *ON
     C     ##PROC        OREQ      *BLANKS
     C                   SETOFF                                       KL
     C     SFRRN         CHAIN     WDW01S                             98
     C                   SETON                                        31
     C  N98              UPDATE    WDW01S
     C                   ELSE
     C     SFRRN         CHAIN     WDW01S                             98
     C                   MOVEL(P)  ##PROC        SC10A1
     C                   SETON                                        32
     C  N98              UPDATE    WDW01S
     C                   ENDIF
     c*
     C     HFLD          WHENEQ    'SC10A2'
     C*
     C                   CALL      'F2015R'
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      *BLANKS       ##PROC
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM      '1'           ##DFT
     C*
     C     *INKL         IFEQ      *ON
     C     ##PROC        OREQ      *BLANKS
     C                   SETOFF                                       KL
     C     SFRRN         CHAIN     WDW01S                             98
     C                   SETON                                        32
     C  N98              UPDATE    WDW01S
     C                   ELSE
     C     SFRRN         CHAIN     WDW01S                             98
     C                   MOVEL(P)  ##PROC        SC10A2
     C*** BEGIN CHG *** 09/08/08 **************************************
     C**********           SETON                     33
     C                   SETON                                        34
     C*** END   CHG *** 09/08/08 **************************************
     C  N98              UPDATE    WDW01S
     C                   ENDIF
     C*** BEGIN ADD *** 09/08/08 **************************************
     c*
     c*  MODIFIER SEARCH 1
     c*
     C     HFLD          WHENEQ    'SCPMOD'
     C                   CALL      'F2315R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      'TX'          ##AST             2
     C                   PARM      *BLANKS       ##KEY            15
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##KEY         OREQ      *BLANKS
     C                   SETOFF                                       KL
     C     SFRRN         CHAIN     WDW01S                             98
     C                   SETON                                        34
     C  N98              UPDATE    WDW01S
     C                   ELSE
     C     SFRRN         CHAIN     WDW01S                             98
     C                   MOVEL     ##KEY         SCPMOD
     C                   SETON                                        35
     C  N98              UPDATE    WDW01S
     C                   ENDIF
     c*
     c*  MODIFIER SEARCH 2
     c*
     C     HFLD          WHENEQ    'SCPMD2'
     C                   CALL      'F2315R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      'TX'          ##AST
     C                   PARM      *BLANKS       ##KEY            15
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##KEY         OREQ      *BLANKS
     C                   SETOFF                                       KL
     C     SFRRN         CHAIN     WDW01S                             98
     C                   SETON                                        35
     C  N98              UPDATE    WDW01S
     C                   ELSE
     C     SFRRN         CHAIN     WDW01S                             98
     C                   MOVEL     ##KEY         SCPMD2
     C                   SETON                                        36
     C  N98              UPDATE    WDW01S
     C                   ENDIF
     c*
     c*  MODIFIER SEARCH 3
     c*
     C     HFLD          WHENEQ    'SCPMD3'
     C                   CALL      'F2315R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      'TX'          ##AST
     C                   PARM      *BLANKS       ##KEY            15
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##KEY         OREQ      *BLANKS
     C                   SETOFF                                       KL
     C     SFRRN         CHAIN     WDW01S                             98
     C                   SETON                                        36
     C  N98              UPDATE    WDW01S
     C                   ELSE
     C     SFRRN         CHAIN     WDW01S                             98
     C                   MOVEL     ##KEY         SCPMD3
     C                   SETON                                        33
     C  N98              UPDATE    WDW01S
     C                   ENDIF
     C*** END   ADD *** 09/08/08 **************************************
     C*
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
     C***** Beg Del ***** 06/23/14 ******************************
     C*****              GOTO      MAINLN
     C***** Beg Del ***** 06/23/14 ******************************
     C                   ENDSR
     OPCO02     E            PCO02E
     OPBD01     E            PBD01E
**  TABLE QCA
OVRDBF FILE(FTABL)   TOFILE(*LIBL/FTABL  )
OVRDBF FILE(FTPMP)   TOFILE(*LIBL/FTPMP  )
DLTOVR FILE(FTABL)
DLTOVR FILE(FTPMP)
**  TABLE UA
-- ADD --
- UPDATE -
-DISPLAY-
**  TABLE TD
NETWORK DISC: Percent of CMS
