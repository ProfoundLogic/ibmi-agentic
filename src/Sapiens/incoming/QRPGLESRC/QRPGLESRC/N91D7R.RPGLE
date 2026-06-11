     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE N91D7R - TYPE D7 DISCOUNT CODE MAINTENANCE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    ALLOWS ENTRY OF SPECIFIC D7 DISCOUNT CODE INFORMATION      *
     H*    (I.E. CODES, AMOUNTS, ANES AMOUNTS)                        *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    N0920R                                                     *
     H*                                                               *
     H*  PROGRAMMER: C:20               REQ/PROJ: NW007               *
     H*  CREATED: 03/07/95                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  09/29/14 C:296 SUPPORT: 86882                                *
     H*                 1) Increase size of screen field #@$2.        *
     H*                    Initial size of screen field 3,0.          *
     H*                    After change size of screen field 5,0.     *
     H*                 2) Record processing is modified, now program *
     H*                    updates/adds only those records which are  *
     H*                    changed.                                   *
     H*                 3) Load the facility rate manually.           *
     H*                                                               *
     H*  09/15/14 C:296 SUPPORT: 86764                                *
     H*                 1) Remove effective date from key of FMPRPUC. *
     H*                 2) Remove validation while "postioning to".   *
     H*                                                               *
     H*  11/30/09 C:079 PROJECT: 114MVC                               *
     H*                 ADD F9 TO TOGGLE FACILITY AND NON-FACILITY    *
     H*                                                               *
     H*  09/01/09 C:198 SUPPORT: 66458                                *
     H*                 REMOVED KEEP MRA REDUCTIONS                   *
     H*                                                               *
     H*  07/07/08 C:191 PROJECT: 114MQE
     H*                 MODIFY FOR WORKNET-NEW NETWORK ALGORITHM
     H*
     H*  02/21/08 C:183 Project: 114MPX                               *
     H*                 Added FA753R call to write Tracking Record    *
     H*
     H*  05/30/06 C:188 PROJECT: 114WMF
     H*                 MADE CORRECTIONS TO SCREEN N91D7F
     H*
     H*  04/19/06 C:163 PROJECT: 114WMF
     H*                 BACK TO BASE - NETWORK DISCOUNT (114MAG)
     H*
     H*  07/12/05 C:132 Project: MMC045  47783
     H*                 ADD MSG TO SCREEN FOR BLANK COMMENT ERROR
     H*                 COMPILE ONLY
     H*
     H*  09/20/00 C:62  Project: NW068
     H*                 -Remove F20=Change status
     H*                 -Remove F23=Delete
     H*
     H*****************************************************************
     FN91D7F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(WDW01S:RRN2)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FNMBDLD    UF A E           K DISK
     FNDCOL     UF A E           K DISK
     F                                     RENAME(PCO01:PCO02)
     F*** BEGIN CHG *** 09/20/00 **************************************
     F**********FMPRL   IF  E           K        DISK
     FFMPRLUC   IF   E           K DISK
     F*** END   CHG *** 09/20/00 **************************************
     FFMDIL     IF   E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D*** BEGIN DEL *** 09/20/00 **************************************
     D**********          CD      1   1 20
     D*** END   DEL *** 09/20/00 **************************************
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
     D***** Beg Chg ***** 09/29/14 ******************************
     DflagNxtchg       s               n   inz(*off)
     D***** End Chg ***** 09/29/14 ******************************

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
     C*** BEGIN ADD *** 04/19/06 **************************************
     C                   KFLD                    SCPMOD
     C*** END   ADD *** 04/19/06 **************************************
     C*
     C     COKY          KLIST
     C                   KFLD                    QQCODE
     C                   KFLD                    YMEFDT
     C                   KFLD                    XXRCDE            5
     C*** BEGIN ADD *** 04/19/06 **************************************
     C                   KFLD                    SCPMOD
     C*** END   ADD *** 04/19/06 **************************************
     C*
     C     COKY2         KLIST
     C                   KFLD                    QQCODE
     C                   KFLD                    YMEFDT
     C*
     C*  CPT4 PROCEDURE CODE FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXCPT4            5
     C***** Beg Del ***** 09/15/14 ************************************
     C*********          KFLD                    YMEFDT
     C***** End Del ***** 09/15/14 ************************************
     C*
     C***** Beg Add ***** 09/29/14 ************************************
     C     COKY3         KLIST
     C                   KFLD                    QQCODE
     C                   KFLD                    YMEFDT
     C                   KFLD                    varCPT4
     C                   KFLD                    varPMOD
     C***** End Add ***** 09/29/14 ************************************
     C*
     C*  ICD9 CODE FILE
     C*
     C     DIKEY         KLIST
     C                   KFLD                    XXICD9            5
     C                   KFLD                    XXLOST            1
     C*
     C     *LIKE         DEFINE    UPDT          YMEFDT
     C***** Beg Chg ***** 09/29/14 ******************************
     C     *LIKE         DEFINE    SCCPT4        varCPT4
     C     *LIKE         DEFINE    SCPMOD        varPmod
     C***** End Chg ***** 09/29/14 ******************************
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
     C*** BEG ADD *** 02/21/08 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** END ADD *** 02/21/08 **************************************
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
     C                   MOVE      'D7'          SCTYPE
     C                   MOVEL     TD(1)         SCTDSC
     C*** BEGIN DEL *** 09/20/00 **************************************
     C**********           MOVEL*BLANKS   SCCMDD
     C*** END   DEL *** 09/20/00 **************************************
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
     C                   Z-ADD     BDAM01        SCAM01
     C                   Z-ADD     BDAM02        SCAM02
     C*** BEG ADD ****** 07/07/08 ************************************
     C*** BEG DEL ****** 09/01/09 ************************************
     C********             MOVE BDLMRA    SCLMRA
     C*** END DEL ****** 09/01/09 ************************************
     C*** END ADD ****** 07/07/08 ************************************
     C*
     C                   Z-ADD     BDEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVE      '0'           *IN41                          UPDATE EXISTING
     C                   MOVE      UA(2)         SCMODE                         RECORD
     C*** BEGIN DEL *** 09/20/00 **************************************
     C********** SCSTAT    IFEQ 'I'
     C**********           MOVE '1'       *IN64
     C**********           MOVELCD,1      SCCMDD
     C**********           ELSE
     C**********           MOVE '0'       *IN64
     C**********           MOVEL*BLANKS   SCCMDD
     C**********           END
     C*** END   DEL *** 09/20/00 **************************************
     C*
     C                   ELSE
     C                   MOVE      QQCODE        SCCODE
     C                   MOVE      *BLANKS       SCCOM1
     C                   MOVE      *BLANKS       SCCOM2
     C                   Z-ADD     *ZEROS        SCAM01
     C                   Z-ADD     *ZEROS        SCAM02
     C*** BEG ADD ****** 07/07/08 ************************************
     C*** BEG DEL ****** 09/01/09 ************************************
     C**********           MOVE *BLANKS   SCLMRA
     C*** END DEL ****** 09/01/09 ************************************
     C*** END ADD ****** 07/07/08 ************************************
     C                   Z-ADD     YMEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVE      '1'           *IN41                          ADD RECORD
     C                   MOVE      UA(1)         SCMODE
     C*** BEGIN DEL *** 09/20/00 **************************************
     C**********           MOVE '0'       *IN64
     C**********           MOVEL*BLANKS   SCCMDD
     C*** END   DEL *** 09/20/00 **************************************
     C                   MOVE      'A'           SCSTAT
     C*
     C                   END
     C*
     C*** BEG ADD *** 11/30/09 **************************************
     C                   SETOFF                                       51
     C*** END ADD *** 11/30/09 **************************************
     C*
     C*** BEG ADD *** 02/21/08 **************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      SCCOM1        HDCOM1           60
     C                   MOVE      SCCOM2        HDCOM2           60
     C                   MOVE      SCAM01        HDAM01            9 2
     C                   MOVE      SCAM02        HDAM02            9 2
     C*** BEG ADD *** 11/30/09 **************************************
     C                   SETON                                        51
     C*** END ADD *** 11/30/09 **************************************
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
     C***** Beg Chg ***** 09/15/14 ******************************
     C******IN11         CASEQ     '1'           FKEYS
     C*****              END                                                      END *IN11
     C                   ExSr      FKEYS
     C***** End Chg ***** 09/15/14 ******************************
     C*
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C*
     C     SCRCDE        IFNE      *BLANKS
     C*
     C*** BEGIN CHG *** 09/20/00 **************************************
     C**********           EXSR EDITDC
     C*** Beg Del *** 09/15/14 **************************************
     C**********  N09              EXSR      EDITDC
     C*** End Del *** 09/15/14 **************************************
     C*** END   CHG *** 09/20/00 **************************************
     C*
     C*** Beg Del *** 09/15/14 **************************************
     C**********         EXSR      WRITDC
     C*** End Del *** 09/15/14 **************************************
     C*
     C                   MOVEL     SCRCDE        XXRCDE
     C                   MOVE      *BLANKS       SCRCDE
     C                   GOTO      TOP
     C                   END                                                    END SCRCDE
     C*
     C                   Z-ADD     RRN2          SVRRN2            5 0
     C*
     C*** Beg Chg *** 09/29/14 **************************************
      /Free
       if flagNxtchg = *on and *in09 = *off;
          exsr srErrRcd;
       endif;
      /End-Free
     C*** End Chg *** 09/29/14 **************************************
     C                   READC     WDW01S                                 50
     C*** Beg Chg *** 09/29/14 **************************************
      /Free
       Dow not %eof(n91d7f);

          If scHCpt <> *blanks or scHPmo <> *blanks;
            varCPT4 = scHCpt;
            varPmod = scHPmo;
          else;
            varCPT4 = scCpt4;
            varPmod = scPmod;
          Endif;

      /End-Free
     C******IN50         IFEQ      '0'
     C*** End Chg *** 09/29/14 **************************************
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
     C*** Beg Chg *** 09/29/14 **************************************
      /Free
       else;                                                              // If scsel = ' '
         If *in09 = *off;
            exsr editDc;
            exsr WRITDC;
         EndIf;
      /End-Free
     C*** End Chg *** 09/29/14 **************************************
     C                   END                                                    END NE ' '
     C                   END                                                    END EQ '4'
     C*** Beg Chg *** 09/29/14 **************************************
     C*****              END                                                    END 0 *IN50
      /Free
            readc WDW01S;
       EndDo;
      /End-Free
     C*** End Chg *** 09/29/14 **************************************
     C*
     C*** BEGIN CHG *** 09/20/00 **************************************
     C**********           EXSR EDITDC
     C*** Beg Del *** 09/29/14 **************************************
     C*****  N09              EXSR      EDITDC
     C*** End Del *** 09/29/14 **************************************
     C*** END   CHG *** 09/20/00 **************************************
     C*
     C*** Beg Del *** 09/29/14 **************************************
     C*****              EXSR      WRITDC
     C*** End Del *** 09/29/14 **************************************
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
     C***** Beg Del ***** 09/29/14 ******************************
     C*****              Z-ADD     *ZEROS        K                 5 0
     C*****NXTDC4        TAG
     C*****              ADD       1             K
     C*****K             CHAIN     WDW01S                             75
     C*****rrn2          CHAIN     WDW01S                             75
     C******IN75         IFEQ      '0'
     C***** End Del ***** 09/29/14 ******************************
     C
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
     C***** Beg Chg ***** 09/29/14 ******************************
     C*****COKY1         CHAIN     PCO02                              60
     C     COKY3         CHAIN     PCO02                              60
     C***** End Chg ***** 09/29/14 ******************************
     C     *IN60         IFEQ      '0'
     C                   DELETE    PCO02
     C*** BEG ADD *** 02/21/08 **************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      'D'           @UWFL1            1
     C                   EXSR      WRTTR1
     C                   ENDIF                                                  CLTID IFEQ 114
     C*** END ADD *** 02/21/08 **************************************
     C                   MOVE      *BLANKS       SCCPT4
KJ   C                   Z-ADD     *ZEROS        SCFEE
     C                   Z-ADD     *ZEROS        SCAAMT
     C*** BEGIN ADD *** 04/19/06 **************************************
     C                   MOVEL     *BLANKS       SCPMOD
     C*** END   ADD *** 04/19/06 **************************************
     C                   Z-ADD     *ZEROS        SCANE
     C                   MOVE      *BLANKS       SCAP
     C                   MOVE      *BLANKS       SCTYP
     C                   MOVE      *BLANKS       SCRPT
     C                   MOVE      *BLANKS       SCDEL
     C     '* DELETE'    CAT       'D'           XXDEL             9
     C                   MOVEL     XXDEL         SCDEL
     C                   MOVE      '1'           *IN08
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C***** Beg Del ***** 09/29/14 ******************************
     C*****              GOTO      MAINLN
     C***** End Del ***** 09/29/14 ******************************
     C                   ELSE
     C                   MOVE      *BLANKS       SCCPT4
     C                   Z-ADD     *ZEROS        SCFEE
     C                   Z-ADD     *ZEROS        SCAAMT
     C*** BEGIN ADD *** 04/19/06 **************************************
     C                   MOVEL     *BLANKS       SCPMOD
     C*** END   ADD *** 04/19/06 **************************************
     C                   Z-ADD     *ZEROS        SCANE
     C                   MOVE      *BLANKS       SCAP
     C                   MOVE      *BLANKS       SCTYP
     C                   MOVE      *BLANKS       SCRPT
     C                   MOVE      *BLANKS       SCDEL
     C     '* DELETE'    CAT       'D'           XXDEL             9
     C                   MOVEL     XXDEL         SCDEL
     C                   MOVE      '1'           *IN08
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C***** Beg Del ***** 09/29/14 ******************************
     C*****              GOTO      MAINLN
     C***** End Del ***** 09/29/14 ******************************
     C                   END
     C                   ELSE
     C     *IN25         IFEQ      '0'
     C                   UPDATE    WDW01S
     C                   END
     C                   END
     C*
     C***** Beg Del ***** 09/29/14 ******************************
     C*****              GOTO      NXTDC4
     C*****              END                                                    END *IN75
     C***** End Del ***** 09/29/14 ******************************
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
     C*** Beg Chg *** 09/29/14 **************************************
      /Free
        flagNxtChg = *on;
      /End-Free
     C*** End Chg *** 09/29/14 **************************************
     C     SCCOM1        IFEQ      *BLANK
     C     SCCOM2        ANDEQ     *BLANK
     C                   MOVE      '1'           *IN24
     C                   MOVE      '1'           *IN26
     C                   GOTO      MAINLN
     C                   END
     C*
     C*** BEG ADD ****** 07/07/08 ************************************
     C*** BEG DEL ****** 09/01/09 ************************************
     C* VALIDATE KEEP MRA REDUCTIONS FLAG
     C*******    SCLMRA    IFNE *BLANK
     C*******    SCLMRA    ANDNE'Y'
     C*******              MOVE '1'       *IN34
     C*******              GOTO MAINLN
     C*******              END
     C*** END DEL ****** 09/01/09 ************************************
     C*** END ADD ****** 07/07/08 ************************************
     C*
     C                   MOVE      RRN2          SRRN2             5 0
     C                   Z-ADD     *ZEROS        K                 5 0
     C*** Beg Chg *** 09/29/14 **************************************
     C*****NXTDC2        TAG
     C*****              ADD       1             K
     C*****K             CHAIN     WDW01S                             75
     C******IN75         IFEQ      '0'
     C*** End Chg *** 09/29/14 **************************************
     C*
     C     SCCPT4        IFEQ      *BLANKS
     C     SCFEE         ANDEQ     *ZEROS
     C     SCANE         ANDEQ     *ZEROS
     C     SCAAMT        ANDEQ     *ZEROS
     C*** BEGIN ADD *** 04/19/06 **************************************
     C     SCPMOD        ANDEQ     *BLANKS
     C*** END   ADD *** 04/19/06 **************************************
     C     SCTYP         ANDEQ     *BLANKS
     C     SCAP          ANDEQ     *BLANKS
     C     SCRPT         ANDEQ     *BLANKS
     C*** Beg Chg *** 09/29/14 **************************************
     C*****              GOTO      NXTDC2
     C                   LeaveSr
     C*** End Chg *** 09/29/14 **************************************
     C                   ELSE
     C*
     C* IF ANYTHING ON LINE ENTERED CPT4 CODE MUST BE ENTERED
     C*
     C     SCCPT4        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN22
     C                   UPDATE    WDW01S
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
     C                   UPDATE    WDW01S
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
     C***** Beg Chg ***** 09/29/14 ******************************
     C*****PRKEY         SETGT     PPR01
     C*****              READP     PPR01                                  60
      /Free
       Chain PRkey PPr01;
          If %found(fmPRluc);
             *in60 = '0';
          else;
             *in60 = '1';
             indErrMsg = '1';
             varMsgTxt = 'Invalid procedure code ' + %trim(SCCPT4) +
                         ' present on the grid';
             Update    WdW01S;
             rrn2 = sRrn2;
             #@$2 = K;
      /End-Free
     C                   GOTO      MAINLN
      /Free
          endif;
      /End-Free
     C***** End Chg ***** 09/29/14 ******************************
     C     *IN60         IFEQ      '0'
     C     PRCO#         ANDEQ     UPCO#
     C     PRCPT4        ANDEQ     XXCPT4
     C                   MOVE      '0'           *IN23
     C                   ELSE
     C                   MOVE      '1'           *IN23
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   END                                                    END *IN60
     C*
     C                   ELSE
     C*
     C                   MOVEL     SCCPT4        XXICD9
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '0'           *IN23
     C                   ELSE
     C                   MOVE      '1'           *IN23
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   END                                                    END *IN60
     C*
     C                   END
     C*
     C* THE A/P FLAG MUST BE A 'A' OR 'P'
     C*
     C     SCAP          IFNE      'A'
     C     SCAP          ANDNE     'P'
     C                   MOVE      '1'           *IN27
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ELSE
     C                   MOVE      '0'           *IN27
     C                   END
     C*
     C* IF A/P FLAG IS A P NOT ANES OR ADDITIONAL AMOUNT ALLOWED
     C*
     C     SCAP          IFEQ      'P'
     C     SCANE         IFGT      *ZEROS
     C     SCAAMT        ORGT      *ZEROS
     C                   MOVE      '1'           *IN29
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ELSE
     C                   MOVE      '0'           *IN29
     C                   END
     C                   ELSE
     C                   MOVE      '0'           *IN29
     C                   END
     C*
     C* THE BY REPORT FLAG MUST BE 'I' OR BLANKS
     C*
     C     SCRPT         IFNE      'I'                                          INDIVIDUAL CONSIDER.
     C     SCRPT         ANDNE     'F'                                          PAY PER STATE FS
     C     SCRPT         ANDNE     *BLANKS
     C                   MOVE      '1'           *IN30
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
     C                   ELSE
     C                   MOVE      '0'           *IN30
     C                   END
     C*
     C     *IN22         IFEQ      '0'
     C     *IN23         ANDEQ     '0'
     C     *IN27         ANDEQ     '0'
     C     *IN28         ANDEQ     '0'
     C     *IN29         ANDEQ     '0'
     C     *IN30         ANDEQ     '0'
     C                   UPDATE    WDW01S
     C*** Beg Chg *** 09/29/14 **************************************
      /Free
        flagNxtChg = *off;
      /End-Free
     C*** End Chg *** 09/29/14 **************************************
     C                   END
     C*
     C*** Beg Chg *** 09/29/14 **************************************
     C*****              GOTO      NXTDC2
     C*** End Chg *** 09/29/14 **************************************
     C                   END                                                    END SCCPT4
     C*** Beg Chg *** 09/29/14 **************************************
     C*****              END                                                    END *IN75
     C*** End Chg *** 09/29/14 **************************************
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
     C*** BEGIN CHG *** 11/30/09 **************************************
     C**********           DO   9         X       50
     C                   DO        9999          X                 5 0
     C*** END   CHG *** 11/30/09 **************************************
     C*
     C     COKY2         READE     PCO02                                  60
     C*
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    PCO02E
     C*
     C                   ADD       1             RRN2              5 0
     C                   MOVEL     COCPT4        SCCPT4
     C                   Z-ADD     COFEE         SCFEE
     C*
     C*** BEG ADD *** 11/30/09 ****************************************
     C     CLTID         IFEQ      '114'
     C     *IN52         ANDEQ     *ON
     C                   Z-ADD     COFFEE        SCFEE
     C                   ENDIF
     C*** END ADD *** 11/30/09 ****************************************
     C*
     C                   Z-ADD     COAAMT        SCAAMT
     C*** BEGIN ADD *** 04/19/06 **************************************
     C                   MOVEL     COPMOD        SCPMOD
     C*** END   ADD *** 04/19/06 **************************************
     C                   Z-ADD     COANES        SCANE
     C                   MOVEL     COFL01        SCTYP
     C                   MOVEL     COFL02        SCAP
     C                   MOVEL     COFL03        SCRPT
     C*** BEG ADD *** 02/21/08 **************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      SCTYP         HDFL01
     C                   MOVE      SCFEE         HDFEE
     C                   MOVE      SCANE         HDANES
     C                   MOVE      SCAAMT        HDAAMT
     C                   MOVE      SCAP          HDFL02
     C                   MOVE      SCRPT         HDFL03
     C                   ENDIF                                                  CLTID IFEQ 114
     C*** END ADD *** 02/21/08 **************************************
     C*
     C*   Save 1st subfile RRN of the current page
     C*
     C     PCFL          IFEQ      'Y'
     C                   Z-ADD     RRN2          XXRRN2            5 0
     C                   MOVE      'N'           PCFL
     C                   END
     C***** Beg Add ****** 09/29/14 ******************************
      /Free
        SChcpt = COcpt4;
        SCHPMO = Copmod;
        SCHTYP = COFL01;
      /End-Free
     C***** End Add ****** 09/29/14 ******************************
     C                   WRITE     WDW01S
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       SCSEL
     C                   MOVE      *BLANKS       SCCPT4
     C                   Z-ADD     *ZEROS        SCFEE
     C                   Z-ADD     *ZEROS        SCAAMT
     C*** BEGIN ADD *** 04/19/06 **************************************
     C                   MOVEL     *BLANKS       SCPMOD
     C*** END   ADD *** 04/19/06 **************************************
     C                   Z-ADD     *ZEROS        SCANE
     C                   MOVE      *BLANKS       SCTYP
     C                   MOVE      *BLANKS       SCAP
     C                   MOVE      *BLANKS       SCRPT
     C*** BEG ADD *** 02/21/08 **************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      SCTYP         HDFL01
     C                   MOVE      SCFEE         HDFEE
     C                   MOVE      SCANE         HDANES
     C                   MOVE      SCAAMT        HDAAMT
     C                   MOVE      SCAP          HDFL02
     C                   MOVE      SCRPT         HDFL03
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
     C***** Beg Add ****** 09/29/14 ******************************
      /Free
        SCHCPT = *blanks;
        SCHPMO = *blanks;
        SCHTYP = *blanks;
      /End-Free
     C***** End Add ****** 09/29/14 ******************************
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
     C     BDKY1         CHAIN     PBD01                              41
     C*
     C***** Beg Del ***** 09/29/14 ************************************
     C*****              Z-ADD     *ZEROS        K                 5 0
     C*****NXTDC3        TAG
     C*****              ADD       1             K
     C*****K             CHAIN     WDW01S                             75
     C*****sRrn2         CHAIN     WDW01S                             75
     C******IN75         IFEQ      '0'
     C***** End Del ***** 09/29/14 ************************************
     C*
     C     SCCPT4        IFEQ      *BLANKS
     C     SCFEE         ANDEQ     *ZEROS
     C     SCANE         ANDEQ     *ZEROS
     C     SCAAMT        ANDEQ     *ZEROS
     C*** BEGIN ADD *** 04/19/06 **************************************
     C     SCPMOD        ANDEQ     *BLANKS
     C*** END   ADD *** 04/19/06 **************************************
     C     SCTYP         ANDEQ     *BLANKS
     C     SCAP          ANDEQ     *BLANKS
     C     SCRPT         ANDEQ     *BLANKS
     C***** Beg Chg ***** 09/29/14 ************************************
     C*****              GOTO      NXTDC3
     C                   LeaveSr
     C***** End Chg ***** 09/29/14 ************************************
     C                   ELSE
     C*
     C***** Beg Chg ***** 09/29/14 ************************************
     C*****COKY1         CHAIN     PCO02                              60
     C     COKY3         CHAIN     PCO02                              60
     C***** End Chg ***** 09/29/14 ************************************
     C*
     C                   MOVEL     SCCODE        COCODE
     C                   Z-ADD     YMEFDT        COEFDT
     C                   MOVEL     SCCPT4        COCPT4
     C***** Beg Chg ***** 09/29/14 ************************************
     C     *in52         IFEQ      *on
     C                   eval      coFfee = scFee
     C                   else
     C***** End Chg ***** 09/29/14 ************************************
     C                   Z-ADD     SCFEE         COFEE
     C***** Beg Chg ***** 09/29/14 ************************************
     C                   endif
     C***** End Chg ***** 09/29/14 ************************************
     C                   Z-ADD     SCAAMT        COAAMT
     C*** BEGIN ADD *** 04/19/06 **************************************
     C                   MOVEL     SCPMOD        COPMOD
     C*** END   ADD *** 04/19/06 **************************************
     C                   Z-ADD     SCANE         COANES
     C                   MOVE      SCTYP         COFL01
     C                   MOVE      SCAP          COFL02
     C                   MOVE      SCRPT         COFL03
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
     C*** BEG ADD *** 02/21/08 **************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      'U'           @UWFL1            1
     C                   EXSR      WRTTR1
     C                   ENDIF                                                  CLTID IFEQ 114
     C*** END ADD *** 02/21/08 **************************************
     C                   ELSE
     C                   MOVEL     Q1USER        COIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         COIDT
     C*
     C                   WRITE     PCO02
     C*** BEG ADD *** 02/21/08 **************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      'W'           @UWFL1            1
     C                   EXSR      WRTTR1
     C                   ENDIF                                                  CLTID IFEQ 114
     C*** END ADD *** 02/21/08 **************************************
     C                   END
     C***** Beg Del ***** 09/29/14 ************************************
     C*****              GOTO      NXTDC3
     C***** End Del ***** 09/29/14 ************************************
     C*
     C                   END                                                    END SCCPT4
     C***** Beg Del ***** 09/29/14 ************************************
     C*****              END                                                    END *IN75
     C***** End Del ***** 09/29/14 ************************************
     C*
     C*  ALL EDITS HAVE BEEN PASSED  *** WRITE/UPDATE FILE!
     C*
     C                   MOVEL     SCCODE        BDCODE
     C                   MOVE      YMEFDT        BDEFDT
     C                   MOVE      SCSTAT        BDSTAT
     C                   MOVE      SCCOM1        BDCOM1
     C                   MOVE      SCCOM2        BDCOM2
     C                   Z-ADD     SCAM01        BDAM01
     C                   Z-ADD     SCAM02        BDAM02
     C                   Z-ADD     *ZEROS        BDPC01
     C                   Z-ADD     *ZEROS        BDPC02
     C                   Z-ADD     *ZEROS        BDPC03
     C                   MOVE      SCTYPE        BDTYPE
     C                   MOVE      'D'           BDRCDT
     C*** BEG ADD ****** 07/07/08 ************************************
     C*** BEG DEL ****** 09/01/09 ************************************
     C********             MOVE SCLMRA    BDLMRA
     C*** END DEL ****** 09/01/09 ************************************
     C*** END ADD ****** 07/07/08 ************************************
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
     C*** BEG ADD *** 02/21/08 **************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      'U'           @UWFLG            1
     C                   EXSR      WRTTR
     C                   ENDIF                                                  CLTID IFEQ 114
     C*** END ADD *** 02/21/08 **************************************
     C                   ELSE
     C                   MOVE      Q1USER        BDIUSR
     C                   WRITE     PBD01
     C*** BEG ADD *** 02/21/08 **************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      'W'           @UWFLG
     C                   EXSR      WRTTR
     C                   ENDIF                                                  CLTID IFEQ 114
     C*** END ADD *** 02/21/08 **************************************
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
     C*** BEG ADD *** 11/30/09 ****************************************
     C*
     C* Toggle between Facility and Non-Facility Allowance
     C*
     C     *INKI         IFEQ      '1'
     C     CLTID         ANDEQ     '114'
     C*
     C     *IN52         IFEQ      *OFF
     C                   SETON                                        52
     C*
     C                   ELSE
     C                   SETOFF                                       52
     C*
     C                   END                                                    in52=off
     C*
     C*** BEGIN ADD *** 11/30/09 **************************************
     C*
     C                   Z-ADD     0             RRN2
     C                   Z-ADD     0             XXRRN2
     C*
     C     COKY          SETLL     PCO02                              60
     C                   MOVE      '1'           *IN04
     C                   WRITE     WDW01C
     C                   MOVE      '0'           *IN04
     C*
     C                   EXSR      LOAD
     C     SFRRN         CHAIN     WDW01S                             99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     SFRRN         #@$2
     C                   UPDATE    WDW01S
     C                   ENDIF
     C*** END   ADD *** 11/30/09 **************************************
     C*
     C***** Beg Add ***** 09/29/14 ************************************
     C                   GOTO      MAINLN
     C***** End Add ***** 09/29/14 ************************************
     C                   END                                                    INKI=1
     C*** END ADD *** 11/30/09 ****************************************
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
     C                   WRITE     WDW01C
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
     C                   MOVEL     'WDW01'       Q2RECF
     C                   GOTO      MAINLN
     C                   END
     C*
     C*** BEGIN DEL *** 09/20/00 **************************************
     C****************************************
     C*   F6 = PROCEDURE CODE SEARCH         *
     C****************************************
     C********** *INKF     IFEQ '1'
     C**********           Z-ADD*ZEROS    K       50
     C********** NXTDC1    TAG
     C**********           ADD  1         K
     C********** K         CHAINWDW01S               75
     C********** *IN75     IFEQ '0'
     C**********           Z-ADD1         T       20
     C********** 1         SUBSTSCCPT4:T  XX1     1    65
     C********** XX1       IFEQ '?'
     C*
     C**********           MOVE *BLANKS   QQCPT4  5
     C**********           MOVE *ZEROS    QQFDAT  80
     C**********           CALL 'F0520R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQCPT4
     C**********           PARM           QQFDAT
     C**********           PARM 'UC'      QQST    2
     C**********           PARM           QQRTCD  1
     C**********           MOVELQQCPT4    SCCPT4
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C**********           Z-ADDK         #@$2
     C**********           UPDATWDW01S
     C**********           GOTO NXTDC1
     C**********           ELSE
     C**********           GOTO NXTDC1
     C**********           END
     C*
     C**********           END
     C**********           END
     C*
     C****************************************
     C*   F7 = ICD-9 CODE SEARCH             *
     C****************************************
     C********** *INKG     IFEQ '1'
     C**********           Z-ADD*ZEROS    K       50
     C********** NXTDC5    TAG
     C**********           ADD  1         K
     C********** K         CHAINWDW01S               75
     C********** *IN75     IFEQ '0'
     C**********           Z-ADD1         T       20
     C********** 1         SUBSTSCCPT4:T  XX1     1    65
     C********** XX1       IFEQ '?'
     C*
     C**********           MOVE *BLANKS   QQICD9  5
     C**********           CALL 'F0510R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQICD9
     C**********           PARM           QQRTCD  1
     C**********           MOVELQQICD9    SCCPT4
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C**********           Z-ADDK         #@$2
     C**********           UPDATWDW01S
     C**********           GOTO NXTDC5
     C**********           ELSE
     C**********           GOTO NXTDC5
     C**********           END
     C*
     C**********           END
     C**********           END
     C*** END   DEL *** 09/20/00 **************************************
     C*
     C     *INKH         IFEQ      '1'
     C                   END
     C*
     C     *INKI         IFEQ      '1'
     C                   END
     C*
     C     *INKJ         IFEQ      '1'
     C*** BEGIN ADD *** 09/20/00 **************************************
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
     C     SFRRN         CHAIN     WDW01S                             75
     C                   SETON                                        36
     C  N75              UPDATE    WDW01S
     C*
     C                   ELSE
     C*
     C     SFRRN         CHAIN     WDW01S                             75
     C                   MOVEL(P)  ##PROC        SCCPT4
     C                   MOVEL     ##RTCD        SCTYP
     C*** BEGIN CHG *** 04/19/06 **************************************
     C******************** SETON                     35
     C                   SETON                                        33
     C*** END   CHG *** 04/19/06 **************************************
     C  N75              UPDATE    WDW01S
     C*
     C                   ENDIF
     C*** END   ADD *** 09/20/00 **************************************
     C                   END
     C*** BEGIN ADD *** 04/19/06 **************************************
     C     *INKJ         IFEQ      '1'
     C     HFLD          ANDEQ     'SCPMOD'
     C*
     C                   CALL      'F2315R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      'UC'          ##AST             2
     C                   PARM      *BLANKS       ##KEY            15
     C*
     C                   WRITE     SCR99
     C*
     C     *INKL         IFEQ      *ON
     C     ##KEY         OREQ      *BLANKS
     C*
     C                   SETOFF                                       KL
     C     SFRRN         CHAIN     WDW01S                             75
     C                   SETON                                        33
     C  N75              UPDATE    WDW01S
     C*
     C                   ELSE
     C*
     C     SFRRN         CHAIN     WDW01S                             75
     C                   MOVEL(P)  ##KEY         SCPMOD
     C*** BEG CHG *** 05/30/07 ****************************************
     C*****                SETON                     35
     C                   SETON                                        42
     C*** END CHG *** 05/30/07 **************************************
     C  N75              UPDATE    WDW01S
     C*
     C                   ENDIF                                                  *INKL=*ON
     C*
     C                   ENDIF                                                  *INKJ='1'
     C*** END   ADD *** 04/19/06 **************************************
     C*
     C     *INKK         IFEQ      '1'
     C                   END
     C*
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01S'
     C     Q2RECF        OREQ      'WDW01C'
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
     C*** BEGIN DEL *** 09/20/00 **************************************
     C****************************************
     C*   F20 = CHANGE STATUS                *
     C****************************************
     C********** *INKU     IFEQ '1'
     C********** *IN40     ANDEQ'0'
     C********** SCSTAT    IFEQ 'A'
     C**********           MOVE 'I'       SCSTAT
     C**********           MOVE '1'       *IN64             ALLOW DELETE
     C**********           MOVELCD,1      SCCMDD
     C**********           ELSE
     C**********           MOVE 'A'       SCSTAT
     C**********           MOVE '0'       *IN64             NO DELETING
     C**********           MOVEL*BLANKS   SCCMDD
     C**********           END
     C**********           GOTO MAINLN
     C**********           END
     C*** END   DEL *** 09/20/00 **************************************
     C*
     C     *INKV         IFEQ      '1'
     C                   END
      *
     C     *INKW         IFEQ      '1'
     C                   END
     C*
     C*** BEGIN DEL *** 09/20/00 **************************************
     C****************************************
     C*   F23 = DELETE CODE                  *
     C****************************************
     C********** *INKX     IFEQ '1'
     C********** *IN64     ANDEQ'1'
     C**********           DELETPBD01
     C********** COKY      CHAINPCO02                60
     C********** *IN60     DOWEQ'0'
     C**********           DELETPCO02
     C********** COKY      READEPCO02                    60
     C**********           END
     C**********           MOVE '1'       *INLR
     C**********           GOTO TERM
     C**********           END
     C*** END   DEL *** 09/20/00 **************************************
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
     C***** Beg Del ***** 09/15/14 **********************************
     C*****              GOTO      MAINLN
     C***** End Del ***** 09/15/14 **********************************
     C                   ENDSR
     C*** BEG ADD *** 02/21/08 **************************************
     C*================================================================
     C* WRTTR - Write Tracking Record
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
     C                   PARM      BDEFDT        @@EFDT            7 0
     C                   PARM      *BLANKS       @@CPT4            5
     C                   PARM      *BLANKS       @@PMOD            2
     C                   PARM      *BLANKS       @@10A1           10
     C                   PARM      *BLANKS       @@10A2           10
     C                   PARM      *BLANKS       @@10A3           10
     C                   PARM      *BLANKS       @@10A4           10
     C                   PARM                    @@FLD            10
     C                   PARM                    @@FVAL           80
     C                   PARM                    @@TVAL           80
     C                   ENDIF                                                  HDCOM1    IFNE
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
     C                   PARM      BDEFDT        @@EFDT
     C                   PARM      *BLANKS       @@CPT4
     C                   PARM      *BLANKS       @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDCOM2    IFNE
      *
     C     HDAM01        IFNE      BDAM01
      *
     C                   MOVEL(P)  'BDAM01'      @@FLD
     C                   MOVEL(P)  HDAM01        @@FVAL
     C                   MOVEL(P)  BDAM01        @@TVAL
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @@EFDT
     C                   PARM      *BLANKS       @@CPT4
     C                   PARM      *BLANKS       @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDAM01    IFNE
      *
     C     HDAM02        IFNE      BDAM02
      *
     C                   MOVEL(P)  'BDAM02'      @@FLD
     C                   MOVEL(P)  HDAM02        @@FVAL
     C                   MOVEL(P)  BDAM02        @@TVAL
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @@EFDT
     C                   PARM      *BLANKS       @@CPT4
     C                   PARM      *BLANKS       @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDAM02    IFNE
      *
     C     @UWFLG        WHENEQ    'W'
      *
     C                   MOVEL(P)  'Added'       @@FLD
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @@EFDT
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
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @EFDT1            7 0
     C                   PARM      COCPT4        @@CPT4
     C                   PARM      COPMOD        @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDFEE IFNE COFEE
      *
     C     HDFL01        IFNE      COFL01
      *
     C                   MOVEL(P)  'COFL01'      @@FLD
     C                   MOVEL(P)  HDFL01        @@FVAL
     C                   MOVEL(P)  COFL01        @@TVAL
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @EFDT1
     C                   PARM      COCPT4        @@CPT4
     C                   PARM      COPMOD        @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDFL01 IFNE COFL01
      *
     C     HDANES        IFNE      COANES
      *
     C                   MOVEL(P)  'COANES'      @@FLD
     C                   MOVEL(P)  HDANES        @@FVAL
     C                   MOVEL(P)  COANES        @@TVAL
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @EFDT1
     C                   PARM      COCPT4        @@CPT4
     C                   PARM      COPMOD        @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDANES IFNE COANES
      *
     C     HDAAMT        IFNE      COAAMT
      *
     C                   MOVEL(P)  'COAAMT'      @@FLD
     C                   MOVEL(P)  HDAAMT        @@FVAL
     C                   MOVEL(P)  COAAMT        @@TVAL
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @EFDT1
     C                   PARM      COCPT4        @@CPT4
     C                   PARM      COPMOD        @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDAAMT IFNE COAAMT
      *
     C     HDFL02        IFNE      COFL02
      *
     C                   MOVEL(P)  'COFL02'      @@FLD
     C                   MOVEL(P)  HDFL02        @@FVAL
     C                   MOVEL(P)  COFL02        @@TVAL
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @EFDT1
     C                   PARM      COCPT4        @@CPT4
     C                   PARM      COPMOD        @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDFL02 IFNE COFL02
      *
     C     HDFL03        IFNE      COFL03
      *
     C                   MOVEL(P)  'COFL03'      @@FLD
     C                   MOVEL(P)  HDFL03        @@FVAL
     C                   MOVEL(P)  COFL03        @@TVAL
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @EFDT1
     C                   PARM      COCPT4        @@CPT4
     C                   PARM      COPMOD        @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDFL03 IFNE COFL03
      *
     C     @UWFL1        WHENEQ    'W'
      *
     C                   MOVEL(P)  'Added'       @@FLD
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @EFDT1
     C                   PARM      COCPT4        @@CPT4
     C                   PARM      COPMOD        @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
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
     C                   PARM      SCCPT4        @@CPT4
     C                   PARM      SCPMOD        @@PMOD
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
     C                   ENDSR                                                  WRTTR1
     C***** Beg Add ***** 09/29/14 ******************************
      /Free
       begsr srErrRcd;

          chain sRrn2 WDW01S;
          if %found(n91d7f);
             if scSel = '4';
                exsr delDsc;
             else;
             exsr editDc;
                if flagNxtchg = *off;
                   if scHCpt <> *blanks or scHPmo <> *blanks;
                     varCPT4 = scHCpt;
                     varPmod = scHPmo;
                   else;
                     varCPT4 = scCpt4;
                     varPmod = scPmod;
                   endif;
                   exsr WRITDC;
                endif;
             endif;
          endif;

       endsr;
      /End-Free
     C***** End Add ***** 09/29/14 ******************************
     C*** END ADD *** 02/21/08 **************************************
     OPCO02     E            PCO02E
     OPBD01     E            PBD01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE TD
NETWORK DISC: CARVE OUT PER CODE
