     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8070R -  PROMPT FOR LOG EXTRACTION PROCESSING
     H*                                                               *
     H*****************************************************************
     H*  CREATED: 06/18/97  for Version 3 BASE
     H*                    C:58  PROJECT: WC468
     H*
     H*   CALLED FROM: MENU
     H*   CALLS TO:          W0870R
     H*                      W8070C
     H*                      W2493F
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/11/19 C:53  Support: 85868                                                      *
     H*                Use Optacomp process for CLTID 153                                  *
     H*
     H* 02/08/14 C:180 SUPPORT PRISS 027 - WC2234
     H*                Re-compiled only.
     H*                                                                       *
     H* 11/06/13 C:180 Support 84069                                  *
     H*                Project 147WBF - Access to other files         *
     H*                using OPTACOMP's admin claim number.           *
     H*                                                               *
     h* 03/18/13 C:282 Redmine: 2455                                  *
     h*                Intialized the default value of schcpy as 'N'  *
     H*                                                               *
     H* 09/28/10 C:220 PROJECT: 139WNN / PRISS#53                     *
     H*                ADD EDI FORMAT ID PARAMETER                    *
     H*                                                               *
     H* 05/14/10 C:235 PROJECT: 139WNN                                *
     H*            220 EDI EXTRACT AND FTP AUTOMATION                 *
     H*                                                               *
     H* 05/21/07 C:191 Support: 48552                                 *
     H*                include records with a status of 'EC' or 'EI'  *
     H*                                                               *
     H* 07/24/06 C:53  PROJECT: WC1434                                *
     H*                ROES ASP integration (RAK1)                    *
     H*                                                               *
     H* 04/27/00 C:53  SUPPORT: WC065/19768                           *
     H*                SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY. *
     H*                                                               *
     H* 07/14/99 C:118 PROJECT:  WC577                                *
     H*                Remove default trans set id. Made V3 chgs to   *
     H*                display file. Added jurisdiction state search. *
     H*                                                               *
     H* 03/23/99 C:15 Project: WC468                                  *
     H*               Added code to update the A49 files WXT2P and    *
     H*               WXT3P.                                          *
     H*****************************************************************
     FW8070F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMM1L     UF A E           K DISK
     FWMLGL3    UF   E           K DISK
     FWXFRL     UF   E           K DISK
     FWXSUL     UF   E           K DISK
     FWXAJL     UF   E           K DISK
     FWXPIL     UF   E           K DISK
     FWXRDL     UF   E           K DISK
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     F*** BEGIN ADD *** 03/23/99 **************************************
     FWXT2L     UF   E           K DISK
     FWXT3L     UF   E           K DISK
     F*** END ADD ***** 03/23/99 **************************************
     F*** BEG ADD ***** 05/14/10 ******************************************
     FWTGTL     IF   E           K DISK
     F*** END ADD ***** 05/14/10 ******************************************
     F*** BEG ADD ***** 09/28/10 ******************************************
     FWMFNL     IF   E           K DISK
     F*** END ADD ***** 09/28/10 ******************************************
     F*** BEG ADD ***** 11/06/13 ******************************************
     FWMCML1    IF   E           K DISK
     F*** END ADD ***** 11/06/13 ******************************************

     D*** BEG ADD *** 11/06/13 ********************************************
     D LGACLC          S             25    INZ
     D*** END ADD *** 11/06/13 ********************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXACLC                 1     25
     D  XXCO#                  1      3  0
     D  XXFND                  4      6  0
     D  XXFYR                  7      9  0
     D  XXCASE                10     16  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR THE STATE TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE SUBMISSION MANAGEMENT FILE
     C*
     C     M1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCJST
     C                   KFLD                    XXEXDT
     C                   KFLD                    XXTSET
     C*** BEG ADD *** 09/28/10 ***********************************************
     C                   KFLD                    XXEDIF            3
     C*
     C     FNKY          KLIST
     C                   KFLD                    XCO#
     C                   KFLD                    XFND
     C*
     C*** END ADD *** 09/28/10 ***********************************************
     C*
     C*   KEY LIST FOR THE 148 SUBMISSION FLAT FILE
     C*
      *** Begin delete ****    11/06/13 ***********************************
     C***  FRKY          KLIST
     C***                KFLD                    XXCO#
     C***                KFLD                    SCJST
     C***                KFLD                    XXEXDT
     C***                KFLD                    XXTSET
     C***                KFLD                    XXACLC
      *** End   delete ****     11/06/13 **********************************

     C*
     C*   KEY LIST FOR THE A49 SUBMISSION FLAT FILE
     C*
      *** Begin delete ****    11/06/13 ***********************************
     C***  SUKY          KLIST
     C***                KFLD                    XXCO#
     C***                KFLD                    SCJST
     C***                KFLD                    XXEXDT
     C***                KFLD                    XXTSET
     C***                KFLD                    XXACLC
      *** End   delete ****     11/06/13 **********************************
     C*
     C*   KEY LIST FOR THE EDI LOG HISTORY FILE
     C*
     C     LGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCJST
     C                   KFLD                    XXEXDT
     C                   KFLD                    XXTSET
     C*** BEG ADD *** 05/14/10 **************************************  ****
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*** END ADD *** 05/14/10 **************************************  ****
      *
     C*** Begin add    ****    11/06/13 ***********************************
     C     FRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCJST
     C                   KFLD                    XXEXDT
     C                   KFLD                    XXTSET
     C                   KFLD                    LGACLC

     C     SUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCJST
     C                   KFLD                    XXEXDT
     C                   KFLD                    XXTSET
     C                   KFLD                    LGACLC

     C     CMKY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
      *** End   add    ****    11/06/13 ***********************************
     C*
     C*   ENTRY PARM LIST
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
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     *LIKE         DEFINE    M1EXDT        XXEXDT
     C     *LIKE         DEFINE    M1TSET        XXTSET
     C*** BEG ADD *** 05/14/10 **************************************  ****
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    UPCO#         XCO#
     C     *LIKE         DEFINE    LGFND         XFND
     C*** END ADD *** 05/14/10 **************************************  ****
     C                   MOVE      *BLANK        XXKEY            10
     C*
     C*
     C     STRT01        TAG
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              62
     C     *IN62         IFEQ      '1'
     C                   Z-ADD     *ZERO         UPCO#
     C                   END
     C                   Z-ADD     UPCO#         XXCO#
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              62
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C***** BEGIN DEL **** 07/14/99 ***********************************
     C**********           MOVE Q1USER    SCUSER
     C***** END   DEL **** 07/14/99 ***********************************
     C                   MOVE      QQPRDS        SCPRDS
     C                   TIME                    DSDTTM
     C*
     C*** BEG ADD *** 12/11/19 ****************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** END ADD *** 12/11/19 ****************************
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*  INITIALIZE STATE AND EXTRACT DATE
     C*
     C                   MOVE      *BLANKS       SCJST
     C***** Beg Chg **** 03/18/13 ***********************************
     C*****              MOVE      *BLANKS       SCHCPY
     C                   MOVE      'N'           SCHCPY
     C***** End Chg **** 03/18/13 ***********************************
     C*
     C***** BEGIN DEL **** 07/14/99 ***********************************
     C**********           MOVEL'1'       SCTSET
     C***** END   DEL **** 07/14/99 ***********************************
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 03 = EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 05 = EXTRACT SUBMISSION SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C*     THE JURISDICTION STATE MUST BE ENTERED AND VALID, BECAUSE
     C*     THIS SEARCH ONLY DISPLAYS A LIST FOR ONE JURS STATE AT A TIME.
     C***** BEGIN ADD **** 07/14/99 ***********************************
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCJST'
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      'JRST'        QQTBID            4
     C                   PARM      *BLANK        XXKEY
     C                   PARM      ' '           QQRTCD            1
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCJST
     C                   MOVE      *ON           *IN29
     C                   ELSE
     C                   GOTO      STRT
     C                   MOVE      *ON           *IN24
     C                   ENDIF                                                  QQRTCD=*
     C*
     C***** END   ADD **** 07/14/99 ***********************************
     C*
     C***** BEGIN DEL **** 07/14/99 ***********************************
     C********** SCJST     CABEQ*BLANKS   STRT           24
     C**********           MOVELSCJST     XXKEY     P
     C***** END   DEL **** 07/14/99 ***********************************
     C     STKY          CHAIN     PST01                              62
     C     *IN62         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C***** BEGIN ADD **** 07/14/99 ***********************************
     C*
     C     HFLD          WHENEQ    'SCEXDT'
     C*
     C     SCJST         IFEQ      *BLANK
     C                   MOVE      *ON           *IN24
     C                   ELSE
     C*
     C***** END   ADD **** 07/14/99 ***********************************
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      SCJST         QQJST             2
     C                   MOVE      SCEXDT        QQEXDT            8
     C                   MOVE      *BLANK        QQRTCD            1
     C                   CALL      'W0870R'
     C                   PARM                    QQCO#
     C                   PARM                    QQJST
     C                   PARM                    QQEXDT
     C                   PARM                    QQRTCD
     C***** BEGIN DEL **** 07/14/99 ***********************************
     C********** QQRTCD    CABEQ'A'       TERM
     C********** QQRTCD    CABEQ'B'       STRT
     C********** QQJST     CABEQ' '       STRT
     C***** END   DEL **** 07/14/99 ***********************************
     C*
     C     QQRTCD        IFEQ      '*'
     C*
     C***** BEGIN DEL **** 07/14/99 ***********************************
     C**********           MOVE QQJST     SCJST
     C***** END   DEL **** 07/14/99 ***********************************
     C*
     C                   MOVE      QQEXDT        SCEXDT
     C*
     C***** BEGIN ADD **** 07/14/99 ***********************************
     C                   MOVE      *ON           *IN30
     C                   ELSE
     C                   MOVE      *ON           *IN29
     C***** END   ADD **** 07/14/99 ***********************************
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C***** BEGIN ADD **** 07/14/99 ***********************************
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCHCPY'
     C                   MOVE      *ON           *IN23
     C*
     C     HFLD          WHENEQ    'SCTSET'
     C                   MOVE      *ON           *IN28
     C*
     C***** BEG   ADD **** 05/14/10 ***********************************
     C*
     C     HFLD          WHENEQ    'SCEDIF'
     C*
     C                   MOVEL(P)  'EDIF'        KKTBID
     C                   MOVE(P)   *BLANK        KKKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL            1
     C                   PARM                    KKTBID
     C                   PARM                    KKKEY
     C                   PARM                    QQRTCD            1
     C                   MOVE      *ON           *IN52                          POS CUSR
     C                   MOVE(P)   *BLANKS       SCEDFD
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  KKKEY         SCEDIF
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCEDFD
     C                   MOVE      *ON           *IN52                          POS CUSR
     C                   ENDIF
     C                   ENDIF
     C*
     C***** END   ADD **** 05/14/10 ***********************************
     C*
     C                   ENDSL
     C*
     C***** END   ADD **** 07/14/99 ***********************************
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C***** BEGIN DEL **** 07/14/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 07/14/99 ***********************************
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*     CF13
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C***** BEGIN DEL **** 07/14/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 07/14/99 ***********************************
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C**  VALIDATE JURISDICTION STATE IS IN TABLE FILE
     C*
     C     SCJST         CABEQ     *BLANKS       STRT                     24
     C                   MOVEL(P)  SCJST         XXKEY
     C     STKY          CHAIN     PST01                              62
     C     *IN62         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C**  VALIDATE THE EXTRACT DATE
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXDT
     C     SRERR         IFNE      '0'
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C**  VALIDATE TRANSACTION SET ID.
     C*
     C     SCTSET        IFNE      '1'
     C     SCTSET        ANDNE     'A'
     C     SCTSET        ANDNE     'B'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   ENDIF
     C*** BEG ADD *** 05/14/10 **************************************  ****
     C*
     C*  The EDI Format ID Must be in the table file if entered.
     C*  If not entered, check the fund file to see if it is required.
     C*
     C     SCEDIF        IFNE      *BLANKS
     C                   MOVEL(P)  'EDIF'        KKTBID
     C                   MOVEL(P)  SCEDIF        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCEDFD
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       SCEDFD
     C                   SETON                                          53
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  If not entered, check the fund file to see if it is required.
     C                   ELSE
     C*
     C* CALL API TO CHECK THE FUND.
     C*
     C                   CALL      'WA075R'
     C                   PARM      UPCO#         XCO#
     C                   PARM      000           XFND
     C                   PARM      *BLANK        QQRTCD
     C     QQRTCD        IFEQ      'F'                                          CODE FOUND
     C                   SETON                                          54      ENTRY REQ'D
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 05/14/10 **************************************  ****
     C*
     C*   AN EXTRACT CANNOT BE RE-SUBMITTED IF THE DATA HAS BEEN DOWNLOADED
     C*   OR THE EXTRACT IS BEING PROCESSED.
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT
     C*
     C*** BEG ADD *** 09/28/10 ***********************************************
     C* ADD EDI FORMAT ID IN KEY LIST FROM SCREEN
     C                   MOVE      SCEDIF        XXEDIF
     C*** END ADD *** 09/28/10 ***********************************************
     C*
     C     SCTSET        IFEQ      'B'
     C     SCTSET        OREQ      '1'
     C                   MOVEL     '148'         XXTSET
     C     M1KY          CHAIN(N)  PM101                              60
     C  N60M1OTDT        IFNE      0
     C     M1STAT        OREQ      'P'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCTSET        IFEQ      'B'
     C     SCTSET        OREQ      'A'
     C                   MOVEL     'A49'         XXTSET
     C     M1KY          CHAIN(N)  PM101                              61
     C  N61M1OTDT        IFNE      0
     C     M1STAT        OREQ      'P'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C**  VALIDATE THE HARDCOPY FLAG
     C*
     C     SCHCPY        IFNE      'Y'
     C     SCHCPY        ANDNE     'N'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  148 PROCESSING
     C*
     C     SCTSET        IFEQ      'B'
     C     SCTSET        OREQ      '1'
     C                   MOVEL     '148'         XXTSET
     C*
     C*   IF THIS IS A VALID RE-SUBMISSION, FIX VARIOUS FILES WHICH ARE
     C*   UPDATED DURING THIS PROCESS.
     C*
     C     M1KY          CHAIN     PM101                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   FIX RECORDS IN THE EDI LOG HISTORY FILE
     C*
     C     LGKY          CHAIN     PLG01                              62
     C     *IN62         DOWEQ     '0'
     C*
     C*** BEG ADD *** 09/28/10 ***********************************************
     C                   MOVEL     LGCO#         XCO#
     C                   MOVEL     LGFND         XFND
     C*
     C     FNKY          CHAIN     PFN01                              62
      /Free
       // ****** Begin Add **** 11/06/13 **************************
          XXFND  = LGFND;
          XXFYR  = LGFYR;
          XXCASE = LGCASE;
          LGACLC = XXACLC;
          Chain CMKY PCM01;
       // *** Begin Chg **** 12/11/19 **************************
       // *** If FNgrpasc = 'OPTACOMP' and CMPDS# <> *Blanks;**//
       If FNgrpasc = 'OPTACOMP' and CMPDS# <> *Blanks
             Or cltID = '152' and CMPDS# <> *Blanks;
       // *** End   Chg **** 12/11/19 **************************
          LGACLC = CMPDS#;
       Endif;
       // ****** End   Add **** 11/06/13 *******************************
      /End-Free
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FNEDIF        X#EDIF            3
     C                   ELSE
     C                   MOVEL     ' '           X#EDIF
     C                   ENDIF
     C*
     C     SCEDIF        IFEQ      X#EDIF
     C*
     C*** END ADD *** 09/28/10 ***********************************************
     C*
     C     LGSTS         IFEQ      'EX'
     C*** BEG ADD *** 07/24/06 ***********************************************
     C     LGSTS         OREQ      'RE'
     C*** END ADD *** 07/24/06 ***********************************************
     C*** BEG ADD ****** 05/21/07 *********************************************
     C     LGSTS         OREQ      'EC'
     C     LGSTS         OREQ      'EI'
     C*** END ADD ****** 05/21/07 *********************************************
     C                   MOVE      'EC'          LGSTS
     C                   MOVEL     Q1USER        LGUSER
     C                   MOVEL     Q1PGM         LGPGM
     C                   MOVE      *ZEROS        LGEXDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        LGTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDT
     C                   UPDATE    PLG01
     C*
     C*** BEGIN ADD *** 03/18/97 ***********************************************
     C*   DELETE THE EXTRACT RECORD FOR THE ENTERED DATE AND
     C*   ASSOCIATED CLAIM.
     C*
     C                   MOVE      LGFND         XXFND
     C                   MOVE      LGFYR         XXFYR
     C                   MOVE      LGCASE        XXCASE
     C     FRKY          CHAIN     PFR01                              61
     C  N61              DELETE    PFR01
     C                   END
     C*** BEG ADD ***** 09/28/10 **************************************
     C                   ENDIF
     C*** END ADD ***** 09/28/10 **************************************
     C*
     C     LGKY          READE     PLG01                                  62
     C                   ENDDO
     C*
     C*   FIX THE SUBMISSION MANAGEMENT RECORD
     C*
     C                   MOVE      'P'           M1STAT
     C                   MOVEL     Q1USER        M1IUSR
     C                   MOVEL     Q1PGM         M1IPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        M1ITM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         M1IDT
     C                   UPDATE    PM101
     C*
     C                   ELSE
     C*
     C*   IF THIS IS AN ORIGINAL SUBMISSION, CREATE THE MANAGEMENT RECORD
     C*
     C                   MOVEL     XXCO#         M1CO#
     C                   MOVEL     SCJST         M1JST
     C                   Z-ADD     XXEXDT        M1EXDT
     C                   MOVE      '148'         M1TSET
     C                   Z-ADD     0             M1OTDT
     C                   Z-ADD     0             M1MTCD
     C                   Z-ADD     0             M1ARCV
     C                   Z-ADD     0             M1AENT
     C                   Z-ADD     0             M1148T
     C                   Z-ADD     0             M1148S
     C                   Z-ADD     0             M1A49T
     C                   Z-ADD     0             M1A49S
     C                   Z-ADD     0             M1OTTM
     C                   MOVE      'P'           M1STAT
     C                   MOVEL     Q1USER        M1IUSR
     C                   MOVEL     Q1PGM         M1IPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        M1ITM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         M1IDT
     C*** BEG ADD *** 09/28/10 ***********************************************
     C                   MOVE      SCEDIF        M1EDIF
     C*** END ADD *** 09/28/10 ***********************************************
     C*
     C                   WRITE     PM101
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  148 PROCESSING
     C*
     C*  A49 PROCESSING
     C*
     C     SCTSET        IFEQ      'B'
     C     SCTSET        OREQ      'A'
     C                   MOVEL     'A49'         XXTSET
     C*
     C*** IF THIS IS A VALID RE-SUBMISSION, FIX VARIOUS FILES WHICH ARE
     C*** UPDATED DURING THIS PROCESS.
     C*
     C     M1KY          CHAIN     PM101                              61
     C     *IN61         IFEQ      '0'
     C*
     C*   FIX RECORDS IN THE EDI LOG HISTORY FILE
     C*
     C     LGKY          CHAIN     PLG01                              62
     C     *IN62         DOWEQ     '0'
     C*
     C*** BEG ADD *** 09/28/10 ***********************************************
     C                   MOVEL     LGCO#         XCO#
     C                   MOVEL     LGFND         XFND
     C*
     C     FNKY          CHAIN     PFN01                              62
      /Free
       // ****** Begin Add **** 11/06/13 *******************************
          XXFND  = LGFND;
          XXFYR  = LGFYR;
          XXCASE = LGCASE;
          LGACLC = XXACLC;
          Chain CMKY PCM01;
       // *** Begin Chg **** 12/11/19 **************************
       // *** If FNgrpasc = 'OPTACOMP' and CMPDS# <> *Blanks;**//
       If FNgrpasc = 'OPTACOMP' and CMPDS# <> *Blanks
             Or cltID = '152' and CMPDS# <> *Blanks;
       // *** End   Chg **** 12/11/19 **************************
          LGACLC = CMPDS#;
       Endif;
       // ****** End   Add **** 11/06/13 *******************************
      /End-Free
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FNEDIF        X#EDIF            3
     C                   ELSE
     C                   MOVEL     ' '           X#EDIF
     C                   ENDIF
     C*
     C     SCEDIF        IFEQ      X#EDIF
     C*
     C*** END ADD *** 09/28/10 ***********************************************
     C*
     C     LGSTS         IFEQ      'EX'
     C*** BEG ADD *** 07/24/06 ***********************************************
     C     LGSTS         OREQ      'RE'
     C*** END ADD *** 07/24/06 ***********************************************
     C*** BEG ADD ****** 05/21/07 *********************************************
     C     LGSTS         OREQ      'EC'
     C     LGSTS         OREQ      'EI'
     C*** END ADD ****** 05/21/07 *********************************************
     C                   MOVE      'EC'          LGSTS
     C                   MOVEL     Q1USER        LGUSER
     C                   MOVEL     Q1PGM         LGPGM
     C                   MOVE      *ZEROS        LGEXDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        LGTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDT
     C                   UPDATE    PLG01
     C*
     C*   DELETE THE EXTRACT RECORDS FOR THE ENTERED DATE AND
     C*   ASSOCIATED CLAIM.
     C*
     C                   MOVE      LGFND         XXFND
     C                   MOVE      LGFYR         XXFYR
     C                   MOVE      LGCASE        XXCASE
     C*
     C     SUKY          CHAIN     PSU01                              61
     C  N61              DELETE    PSU01
     C*
     C     *IN62         DOUEQ     '1'
     C     SUKY          DELETE    PAJ01                              62
     C                   END
     C*
     C     *IN62         DOUEQ     '1'
     C     SUKY          DELETE    PPI01                              62
     C                   END
     C*
     C     *IN62         DOUEQ     '1'
     C     SUKY          DELETE    PRD01                              62
     C                   END
     C*
     C*** BEGIN ADD *** 03/23/99 **************************************
     C     *IN62         DOUEQ     '1'
     C     SUKY          DELETE    PT201                              62
     C                   END
     C*
     C     *IN62         DOUEQ     '1'
     C     SUKY          DELETE    PT301                              62
     C                   END
     C*** END ADD ***** 03/23/99 **************************************
     C*
     C                   END
     C*** BEG ADD ***** 09/28/10 **************************************
     C                   ENDIF
     C*** END ADD ***** 09/28/10 **************************************
     C*
     C     LGKY          READE     PLG01                                  62
     C                   ENDDO
     C*
     C*   FIX THE SUBMISSION MANAGEMENT RECORD
     C*
     C                   MOVE      'P'           M1STAT
     C                   MOVEL     Q1USER        M1IUSR
     C                   MOVEL     Q1PGM         M1IPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        M1ITM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         M1IDT
     C                   UPDATE    PM101
     C*
     C                   ELSE
     C*
     C*   IF THIS IS AN ORIGINAL SUBMISSION, CREATE THE MANAGEMENT RECORD
     C*
     C                   MOVEL     XXCO#         M1CO#
     C                   MOVEL     SCJST         M1JST
     C                   Z-ADD     XXEXDT        M1EXDT
     C                   MOVE      'A49'         M1TSET
     C                   Z-ADD     0             M1OTDT
     C                   Z-ADD     0             M1MTCD
     C                   Z-ADD     0             M1ARCV
     C                   Z-ADD     0             M1AENT
     C                   Z-ADD     0             M1148T
     C                   Z-ADD     0             M1148S
     C                   Z-ADD     0             M1A49T
     C                   Z-ADD     0             M1A49S
     C                   Z-ADD     0             M1OTTM
     C                   MOVE      'P'           M1STAT
     C                   MOVEL     Q1USER        M1IUSR
     C                   MOVEL     Q1PGM         M1IPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        M1ITM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         M1IDT
     C*** BEG ADD *** 09/28/10 ***********************************************
     C                   MOVE      SCEDIF        M1EDIF
     C*** END ADD *** 09/28/10 ***********************************************
     C*
     C                   WRITE     PM101
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  A49 PROCESSING
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      XXCO#         QQCO#
     C                   MOVEL     SCJST         QQJST
     C                   MOVE      SCEXDT        QQEXDT
     C                   CALL      'W8070C'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQJST             2
     C                   PARM                    QQEXDT            8
     C                   PARM                    SCHCPY
     C                   PARM                    SCTSET
     C*** BEG ADD *** 05/14/10 **************************************  ****
     C                   PARM                    SCEDIF
     C*** END ADD *** 05/14/10 **************************************  ****
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
