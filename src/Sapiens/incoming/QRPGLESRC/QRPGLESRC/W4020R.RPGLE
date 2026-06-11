     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W4020R - FEDERAL ID# CHANGE
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*                                                              *
     H* 07/22/22   C:300 Project: WC2395                             *
     H*                  Changes to increase FEIN field width in DSPF*
     H*                  New Agency Maintenance screen               *
     H*                                                              *
     H* 06/15/22   C:300 Project: WC2395                             *
     H*                  Change layout SCR01 record format for       *
     H*                  New Agency Maintenance screen               *
     H*                                                              *
     H* 07/14/14   C:295 Project: CI103                              *
     H*                  Update Agent details to WCMS to FQCIC       *
     H*                                                              *
     H*  12/27/11 C:282 Project: WC2005                              *
     H*                Add API WA085R to change EFT payee name/FEIN  *
     H*                                                              *
     H*  07/28/04  C:132 PROJECT: WA099   43301                      *
     H*                - Add change of Agent FEIN, MOD in WMACP file *
     H*                  by reading file by Group, Agent nbr.        *
     H*                - The WMACP FEIN and MOD must match the old   *
     H*                  WMAGP FEIN and MOD to be updated or it      *
     H*                  can be Zeros and be updated.                *
     H*                                                              *
     H*  12/16/97  C:33  PROJECT: WC683                              *
     H*                  V3 REWRITE (NO COMMENTS)                    *
     H*                                                              *
     H*  03/03/98  C:86  PROJECT: WC822                              *
     H*                  ADDED A CALL TO SC900R SO THAT DARTS REMARKS*
     H*                  WILL BE UPDATED IF FEDERAL ID # IS CHANGED  *
     H****************************************************************
     FW4020F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMAGL     IF   E           K DISK
     F                                     RENAME(PAG01:PAG02)
     FWMAAL     IF   E           K DISK
     F                                     RENAME(PAA01:PAA02)
     F                                     INFDS(DBFDSA)
     FWMAPL     IF   E           K DISK
     F                                     RENAME(PAP01:PAP02)
     F                                     INFDS(DBFDSP)
     FWMPSL     IF   E           K DISK
     F                                     RENAME(PPS01:PPS02)
     F                                     INFDS(DBFDSS)
      *
      ***** BEG ADD ***** 07/28/04 *********************************
      *
      * Agent Commission File
     FWMACL3    UF   E           K DISK
      ***** END ADD ***** 07/28/04 *********************************
      *
     FWMAGP     UF   E             DISK
     F*** BEGIN ADD ***  12/27/11  ****************************************
     F                                     INFDS(DBFDSG)
     F*** END ADD *****  12/27/11  ****************************************
     FWMAAP     UF   E             DISK
     FWMAPP     UF   E             DISK
     FWMPSP     UF   E             DISK

     D*********  Beg Add ******** 07/14/14 ****************************
     DciAgtChg         pr                  extpgm('CIAGTCHG')
     D @@Rrn                          9  0
     D @@Type                        10
     D @@OFein                        9  0
     D @@OMod                         3  0
     D @@OName                      100
     D*********  End Add ******** 07/14/14 ****************************
     D*
     D OPTNM           C                   CONST('Modify Agent Federal-
     D                                      Id Number')
     D*
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D DBFDSP          DS
     D  DBRRNP               397    400B 0
     D DBFDSS          DS
     D  DBRRNS               397    400B 0
     D*** BEGIN ADD ***  12/27/11  ************************************
     D DBFDSG          DS
     D  DBRRNG               397    400B 0
     D*** END ADD *****  12/27/11  ************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  QFKEY                  1     40
     D  QFFED#                 1      9
     D  QFMOD                 10     12
     D                 DS
     D  QTKEY                  1     40
     D  QTFED#                 1      9
     D  QTMOD                 10     12
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D*********  Beg Add ******** 07/14/14 ****************************
     D @@Rrn           s              9  0
     D @@Type          s             10
     D @@OFein         s              9  0
     D @@OMod          s              3  0
     D @@OName         s            100
     D*********  Beg Add ******** 07/14/14 ****************************
     C*
     C     *LIKE         DEFINE    AGCO#         XXCO#
     C     *LIKE         DEFINE    AGFEIN        XXFEIN
     C     *LIKE         DEFINE    AGMOD         XXMOD
     C     *LIKE         DEFINE    AGDT          XXDT
     C*
      *
      ***** BEG ADD ***** 07/28/04 *********************************
     C*  AGENT MASTER FILE
     C*
     C     ACKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFND
     C                   KFLD                    AAAGT#
      ***** END ADD ***** 07/28/04 *********************************
      *
     C*  AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFEI2
     C                   KFLD                    SCMOD2
     C*
     C*  CHANGE KLIST
     C*
     C     CHGKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
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
     C*_______________________________________________________________
     C*
     C     @@RRN         CHAIN(N)  WMAGP                              60
     C*
     C                   Z-ADD     AGCO#         XXCO#
     C                   Z-ADD     AGFEIN        SCFEIN
     C                   Z-ADD     AGMOD         SCMOD
     C                   MOVEL(P)  AGNAME        SCNAME
     C                   MOVE      AGSTAT        SCSTAT
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     AGCO#         CHAIN     PCO01                              60
     C*
     C                   MOVEL(P)  CONAME        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL(P)  SRDFLD        SCCONM
     C*
     C                   MOVEL(P)  OPTNM         SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL(P)  SRDFLD        SCPRDS
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C     @@MODE        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF                                                  Q2RECF=SCR01
     C*_______________________________________________________________
     C*
     C*  PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*===============================================================
     C*  F3=EXIT
     C*===============================================================
     C*
     C     *INKC         CABEQ     *ON           TERM                     LR
     C*
     C*===============================================================
     C*  F12=CANCEL
     C*===============================================================
     C*
     C     *INKL         CABEQ     *ON           TERM                     LR
     C*
     C                   ENDIF                                                  *IN11=*ON
     C*_______________________________________________________________
     C*
     C*  GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*_______________________________________________________________
     C*
     C*  PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  FEDERAL ID# CANNOT LESS THAN OR EQUAL ZERO
     C*
     C     SCFEI2        CABLE     *ZERO         STRT                   2020
     C*
     C*  THE MODIFIER CANNOT BE LESS THAN ZERO
     C*
     C     SCMOD2        CABLT     *ZERO         STRT                   21
     C*
     C*  THE NEW FEDERAL ID CANNOT ALREADY EXIST
     C*
     C     AGKY          CHAIN     WMAGL                              60
     C*
     C     *IN60         CABEQ     *OFF          STRT                     22
     C*
     C*  ALL EDITS PASSED, UPDATE RECORDS
     C*_______________________________________________________________
     C*
     C     @@RRN         CHAIN     WMAGP                              60
     C*
     C                   Z-ADD     AGFEIN        XXFEIN
     C                   Z-ADD     AGMOD         XXMOD
     C*
     C                   Z-ADD     SCFEI2        AGFEIN
     C                   Z-ADD     SCMOD2        AGMOD
     C                   MOVEL(P)  Q1PGM         AGPGM
     C                   MOVEL(P)  Q1USER        AGUSER
     C                   Z-ADD     DSTIME        AGTIME
     C                   Z-ADD     XXDT          AGDT
     C*
     C  N60              UPDATE    PAG01
     C*
     C*  UPDATE AGENT ASSIGNMENT FILE
     C*
     C     CHGKY         CHAIN     WMAAL                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     DBRRNA        CHAIN     WMAAP                              60
     C*
     C                   Z-ADD     SCFEI2        AAFEIN
     C                   Z-ADD     SCMOD2        AAMOD
     C                   MOVEL(P)  Q1PGM         AAPGM
     C                   MOVEL(P)  Q1USER        AAUSER
     C                   Z-ADD     DSTIME        AATIME
     C                   Z-ADD     XXDT          AADT
      *
      ***** BEG ADD ***** 07/28/04 *********************************
      * Read WMACP file for same Original FEIN or blank FEIN, then
      * Update the Agent Commission File with new FEIN and MOD
     C     ACKY          SETLL     PAC01
     C     ACKY          READE     PAC01                                  61
     C     *IN61         DOWEQ     *OFF
     C     ACFEIN        IFEQ      XXFEIN
     C     ACMOD         ANDEQ     XXMOD
     C     ACFEIN        OREQ      *ZEROS
     C                   Z-ADD     SCFEI2        ACFEIN
     C                   Z-ADD     SCMOD2        ACMOD
     C                   UPDATE    PAC01
     C                   ENDIF
     C     ACKY          READE     PAC01                                  61
     C                   ENDDO
      ***** END ADD ***** 07/28/04 *********************************
      *
     C*
     C  N60              UPDATE    PAA01
     C*
     C     CHGKY         READE     WMAAL                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C*  UPDATE AGENT PRODUCER FILE
     C*
     C     CHGKY         CHAIN     WMAPL                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     DBRRNP        CHAIN     WMAPP                              60
     C*
     C                   Z-ADD     SCFEI2        APFEIN
     C                   Z-ADD     SCMOD2        APMOD
     C                   MOVEL(P)  Q1PGM         APPGM
     C                   MOVEL(P)  Q1USER        APUSER
     C                   Z-ADD     DSTIME        APTIME
     C                   Z-ADD     XXDT          APDT
     C*
     C  N60              UPDATE    PAP01
     C*
     C     CHGKY         READE     WMAPL                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C*  UPDATE PRODUCER NON-RESIDENT FILE
     C*
     C     CHGKY         CHAIN     WMPSL                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     DBRRNS        CHAIN     WMPSP                              60
     C*
     C                   Z-ADD     SCFEI2        PSFEIN
     C                   Z-ADD     SCMOD2        PSMOD
     C                   MOVEL(P)  Q1PGM         PSPGM
     C                   MOVEL(P)  Q1USER        PSUSER
     C                   Z-ADD     DSTIME        PSTIME
     C                   Z-ADD     XXDT          PSDT
     C*
     C  N60              UPDATE    PPS01
     C*
     C     CHGKY         READE     WMPSL                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C******* BEGIN ADD ********* 03/03/98 ****************************
     C*
     C*  UPDATE THE REMARKS CODES TO FOLLOW THE NEW FEDERAL #
     C*
     C                   MOVE      *BLANK        QQRTCD            1
     C                   MOVE      SCFEIN        QFFED#
     C                   MOVE      SCFEI2        QTFED#
     C                   MOVE      SCMOD         QFMOD
     C                   MOVE      SCMOD2        QTMOD
     C*
     C                   CALL      'SC900C'
     C                   PARM                    QFKEY
     C                   PARM                    QTKEY
     C                   PARM                    QQQUE             1 0
     C                   PARM                    QQMODE            1
     C                   PARM                    QQRTCD
     C******* BEGIN ADD ********* 03/03/98 ****************************
     C*** BEGIN ADD ***  12/27/11  ****************************************
     C                   CALL      'WA085R'
     C                   PARM      'A'           XXTYPE            1
     C                   PARM      DBRRNG        XXRRN             9 0
     C                   PARM      *BLANK        XXRTN             1
     C*** END ADD *****  12/27/11  ****************************************
     C*
     C*********  Beg Add ******** 07/14/14 ****************************
      /Free
           @@Rrn  = DBrrng;
           @@Type = 'FEIN';
           @@OFein= XXFEIN;
           @@OMod = XXMOD;
           @@OName= AGname;
           CIAGTCHG (@@Rrn:@@Type:@@OFein:@@OMod:@@OName);
      /End-Free
     C*********  End Add ******** 07/14/14 ****************************
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C*
     C                   MOVE      *ON           *INLR
