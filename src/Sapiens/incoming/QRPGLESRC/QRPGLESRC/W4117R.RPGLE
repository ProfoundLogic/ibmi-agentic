     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR  dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H BNDDIR('WTBNDDIR')
     H*
     H/TITLE W4117R - CREATE POLICY LETTER WORKFILE FOR PRINTING
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION                                           *
     H* THIS PGM CREATES THE WORKFILE WHICH WILL BE USED IN PRTDOC CMD
     H*                                                               *
     H* This program processes file WSDLP created in the prior step & *
     H* builds an additional workfile record before executing the     *
     H* PRTDOC command.  The program also writes a DARTS remark       *
     H* which points to the Policy letter that was generated.         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     W4115C                                                    *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*     QCMDEXC   (PRTDOC)                                        *
     H*     SRNAME                                                    *
     H*     SRFBLD                                                    *
     H*     SRDBLD                                                    *
     H*     SRFRLS                                                    *
     H*                                                               *
     H*  PROGRAMMER: C:30                                             *
     H*                                                               *
     H*  CREATED:    11/28/94               PROJECT: WC375            *
     H* ***************************************************************
     H*                                                               *
     H* INDICATOR USAGE                                               *
     H*  50    GENERAL CHAIN/USAGE                                    *
     H*  60    READ/DOWEQ ON WSDLL (PDL01)                            *
     H*  65    READ/DOWEQ ON WDELL (PEL01)                            *
     H*                                                               *
     H* ***************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/03/14  C:62  Support: WA093 86879                          *
     H*                 Populate user phone numbers.                  *
     H*                                                               *
     H* 09/16/14 C:180 SUPPORT: WA129 86575                           *
     H*                Truncate agent name to 30 char and             *
     H*                agent address to 34 char.                      *
     H*                                                               *
     H* 07/10/14 C:65  Project: WC2277                                *
     H*                Call the generate notice procedure for         *
     H*                creating records for a notification email.     *
     H*                                                               *
     H*  01/21/13  C:287 SEQ#: SY266_OVR                              *
     H*                  Add default activation group compiler options*
     H*
     H* 05/22/06  C:02   PROJECT: XXXXXX                              *
     H*                  ALLOW MSWORD EDTDTMDOC OR DSPDTMDO.         *
     H*
     H* 07/07/05 C: 58 PROJECT: WC1205
     H*              - Access activity status code from employer
     H*                detail WDELP rather than master WMEMP.
     H*                                                               *
     H* 01/30/04 C:136 Project: SY059                                 *
     H*                Replace SYCHGDOC with SYCHGDA for DTM.         *
     H*                                                               *
     H* 01/26/04 C:15  PROJECT: SY059                                 *
     H*                DTM REQUIRES JOBQ(*NO) ON SYPRTDOC COMMAND.    *
     H*                                                               *
     H* 11/18/02 C:61  PROJECT WC1088                                 *
     H*                Add user email to department letter workfiles. *
     H*                                                               *
     H* 01/31/02 C:61  PROJECT WC1070                                 *
     H*                Get Employer Contact info from WMECP.          *
     H*                                                               *
     H* 03/28/01 C:90  PROJECT: WC577                                 *
     H*                ADD CARRIER POLICY # TO DEPT. LETTER.          *
     H*                                                               *
     H* 03/01/01 C:69  Project: SY033
     H*                Change OV commands to use external APIs
     H*                                                               *
     H* 08/15/00 C:53  PROJECT: WC577
     H*                Swap out wtscl for the generic table file
     H*                                                               *
     H*  04/26/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*
     H* 08/05/99 C:61  PROJECT WC857                                  *
     H*              - Add Agent Fax number from Agent Master File    *
     H*                (WMAGP) to Employer Department Letter Workfile *
     H*                (WXDLP).                                       *
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H*  09/01/98 C:02 Project: 33WDQ                                 *
     H*               -DARTS NOT WRITTEN FOR BLANK RMCD
     H*                                                               *
     H*                                                               *
     H*  05/28/98 C:77 Project: WC577                                 *
     H*               -ADD PROCESSING TO SUPPORT OPTION OF WHETHER OR *
     H*                NOT TO ALLOW CHANGES TO DOCUMENTS RESOLVED TO  *
     H*                DARTS.                                         *
     H*               -REMOVE ALL REFERENCES TO AREA OF SERVICE AND   *
     H*                SUBAREA.                                       *
     H*                                                               *
     H*  05/27/98 C:09 Project: WC577                                 *
     H*                USE SYSTEM FOLDER BUILD INSTEAD OF CURRENT     *
     H*                AUTO FOLDER BUILD.                             *
     H*                                                               *
     H*  05/13/98 C:65 Project: DR048/11595                           *
     H*                USE THE REMARK SETTING FOR DIARY FLAG.         *
     H*                                                               *
     H*  02/23/98 C:85 Project: WC577                                 *
     H*                CHANGED THE FIELD MOVED TO CMSMCD              *
     H*                FROM *BLANKS TO CTSMCD.                        *
     H*                                                               **
     H*  09/05/97 C:47  - PROJ: WC577                                 **
     H*                  -ADDED 46 NEW FIELDS AND CODE TO LOAD THEM   *
     H*                  -UPDATED TO VERSION 3 SPECS                  *
     H*                                                               **
     H*  12/15/95 C:33  - PROJ: SUPT                                  **
     H*                  -MODIFICATION TO GET SEQUENCE NUMBER DIRECTLY*
     H*                   BEFORE WRITING DARTS REMARK                 *
     H*                  -ADDED FILE SDCMLI(CMKEY/CMSEQ# ONLY)        *
     H*                                                               *
     H* 09/01/95 C:11 PROJECT WC376                                   *
     H*            1. SPECIFY SELECTED OUTQ ON PRTDOC COMMAND.        *
     H*            2. FIX: REMOVE QUOTE FILES.                        *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H* ***************************************************************
     FWSDLL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMEML     IF   E           K DISK
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      * Employer Contact
     FWMECL1    IF   E           K DISK
      ***** END ADD ******* 01/31/02 **********************************
     FWMREL     IF   E           K DISK
     FWMCPL     IF   E           K DISK
     F*** BEG DEL *** 08/15/00 **********************************
     F***WTSCL   IF  E           K        DISK
     F*** END DEL *** 08/15/00 **********************************
     FWTGTL     IF   E           K DISK
     FWTAVL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PRC01)
     FWDELL1    IF   E           K DISK
     F*** begin add ***  07/10/14  ********************************************
     F                                     infds(dbfds)
     FSDCMLA    IF   E           K DISK    rename(pcm01:pcm0a)
     F*** end add *****  07/10/14  ********************************************
     FWMAHL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWMDOL     IF   E           K DISK
     FWXDLP     O    E           K DISK
     F                                     RENAME(PDL01:PDL02)
     FWXDLL     UF   E           K DISK
     F                                     RENAME(PDL01:PDL03)
     FWXD2L     UF A E           K DISK
     F*** BEGIN DEL *** 04/26/00 **************************************
     F********SDCML   O   E           K        DISK
     F********SDCMLI  IF  E           K        DISK
     F********    PCM01                             KRENAMEPCM0I
     F*** END DEL *** 04/26/00 ****************************************
     FWMUPL1    IF   E           K DISK
      ***** BEG ADD ******* 11/18/02 **********************************
     FWMUPL     IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
      ***** END ADD ******* 11/18/02 **********************************
     FWMAPL     IF   E           K DISK
     FWMCML     IF   E           K DISK
     F                                     RENAME(PCM01:PCM03)
     F*                                                                *********
     F*** BEGIN ADD *** 02/23/98 ***********************************************
     FSTCTL1    IF   E           K DISK
     F*** END   ADD *** 02/23/98 ***********************************************
     F*** BEGIN ADD *** 05/27/98 ***********************************************
     FSSYLF     IF   E           K DISK
     F*** END ADD ***** 05/27/98 ***********************************************
     F**** BEGIN ADD **** 03/28/01 ************************************
     FWMRPL     IF   E           K DISK
     F**** END   ADD **** 03/28/01 ************************************
     F*                                                                *********

     D CMD             S              1    DIM(225) CTDATA PERRCD(75)           QCMDEXC COMMAND
     D CM2             S              1    DIM(225) CTDATA PERRCD(75)           QCMDEXC COMMAND
*****D***ADD 5/22/06 **************************************************
     D CM3             S              1    DIM(225) CTDATA PERRCD(75)           QCMDEXC COMMAND
*****D* END ADD 5/22/06 **************************************************
     D FLR             S              1    DIM(80)                              FOLDER ARRAY
     D CNB             S              1    DIM(40) ASCEND                       COMPANY NAME W/ BLAN
     D CN              S              1    DIM(40)                              COMPANY NAME
     D FNB             S              1    DIM(40) ASCEND                       FUND NAME W/ BLANKS
     D FN              S              1    DIM(40)                              FUND NAME
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D*** BEGIN DEL *** 04/26/00 **************************************
     D********PCM0I
     D********      CMKEY                           PIKEY
     D********      CMSEQ#                          PISEQ#
     D*** END DEL *** 04/26/00 ****************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D                 DS
     D  CX                     1     30
     D  XXCLEX                 1     30
     D                 DS
     D  XXSS#                  1      9
     D  CFSS1                  1      3
     D  CFSS2                  4      5
     D  CFSS3                  6      9
     D**  DATA STRUCTURE FOR DARTS RECORD ****************************
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTFL                40     40
     D*
     D* DATA STRUCTURE FOR DARTS RECORD (AGENT)
     D*
     D                 DS
     D  QAKEY                  1     40
     D  QAFEIN                 1      9  0
     D  QAMOD                 10     12  0
     D**  DATA STRUCTURE FOR EMPLOYER MASTER EFFECTIVE DATES *********
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D**  DATA STRUCTURE FOR EMPLOYER MASTER CANCELLATION DATES ******
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** begin add ***  07/10/14  *********************************************
     D*
     D dbfds           ds
     D  dbrrn                397    400b 0
     D*
     d x#RRN           s              9  0 inz(0)
     d x#seq#          s              5  0 inz(0)
     d x#Dcid          s             12    inz(' ')
     d x#Fold          s             80    inz(' ')
     d x#Uncn          s            255    inz(' ')
     d x#Key           c                   const('Cover Letter')
     D x#Type          c                   const('Policy Set')
     D x#Mode          c                   const('E')
     d*
     D/copy sPrNotice
     D*** end add *****  07/10/14  *********************************************
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      *
      *===============================================================
      * KLIST
      *===============================================================
      *
      *  Employer Contact
      *
      ***** BEG CHG ******* 11/18/02 **********************************

     C*********  ECKY      KLIST
     C*********            KFLD           EMCO#
     C*********            KFLD           EMFND
     C*********            KFLD           EMEMP#
     C*********            KFLD           EMDIV
     C*********            KFLD           KKPCNT  1
     C     ECKY          KLIST
     C                   KFLD                    DLCO#
     C                   KFLD                    DLFND
     C                   KFLD                    DLEMP#
     C                   KFLD                    DLDIV
     C                   KFLD                    KKPCNT            1
      ***** END CHG ******* 11/18/02 **********************************
      *
      ***** END ADD ******* 01/31/02 **********************************
     C*
     C*  AGENT ASSIGNMENT FILE
     C     AAKEY         KLIST
     C                   KFLD                    DLCO#
     C                   KFLD                    DLFND
     C                   KFLD                    DLAAGT
     C*
     C*  AGENT MASTER FILE
     C     AGKEY         KLIST
     C                   KFLD                    DLCO#
     C                   KFLD                    DLAFID
     C                   KFLD                    DLAMOD
     C*
     C*  AGENT HISTORY FILE
     C     AHKEY         KLIST
     C                   KFLD                    DLCO#
     C                   KFLD                    DLFND
     C                   KFLD                    DLEMP#
     C                   KFLD                    DLDIV
     C                   KFLD                    DLFYR
     C*
     C*  AGENT PRODUCER FILE
     C     APKEY         KLIST
     C                   KFLD                    DLCO#
     C                   KFLD                    DLAFID
     C                   KFLD                    DLAMOD
     C                   KFLD                    DLAPRO
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    DLCO#
     C                   KFLD                    DLFND
     C                   KFLD                    DLEMP#
     C                   KFLD                    DLDIV
     C*
     C*  COMPANY FILE
     C     COKEY         KLIST
     C                   KFLD                    DLCO#
     C*
     C*  KEY LIST FOR CHAPTER
     C*
     C     CPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    DLFND
     C                   KFLD                    EMCHAP
     C*** BEGIN DEL *** 04/26/00 **************************************
     C***DARTS FILE
     C********   C2KY      KLIST
     C********             KFLD           QCKEY
     C********             KFLD           ##SEQ#
     C********
     C********   C3KY      KLIST
     C********             KFLD           QAKEY
     C********             KFLD           ##SEQ#
     C*** END DEL *** 04/26/00 ****************************************
     C*** begin add ***  07/10/14  ********************************************
     C*
     C* Darts File
     C*
     C     keySDCMLA     KLIST
     C                   KFLD                    q#Key
     C                   KFLD                    x#Seq#
     C*** end add *****  07/10/14  ********************************************
     C*
     C*  DEPT LETTERS WORKFILE - SCREEN
     C     DLKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  DEPT/DOC MASTER FILE
     C     DOKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDEPT
     C                   KFLD                    DLDPCD
     C                   KFLD                    DLFLDR
     C                   KFLD                    DLDCID
     C*
     C*  EMPLOYER DETAIL FILE
     C     ELKEY         KLIST
     C                   KFLD                    DLCO#
     C                   KFLD                    DLFND
     C                   KFLD                    DLFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     ELKEY1        KLIST
     C                   KFLD                    DLCO#
     C                   KFLD                    DLFND
     C                   KFLD                    DLFYR
     C*
     C*  EMPLOYER MASTER FILE
     C     EMKEY         KLIST
     C                   KFLD                    DLCO#
     C                   KFLD                    DLFND
     C                   KFLD                    DLEMP#
     C                   KFLD                    DLDIV
     C*
     C     EMKY2         KLIST
     C                   KFLD                    DLCO#
     C                   KFLD                    DLFND
     C*
     C*  FUND MASTER FILE
     C     FNKEY         KLIST
     C                   KFLD                    DLCO#
     C                   KFLD                    DLFND
     C*
     C*   FUND AUXILIARY FILE
     C     F2KEY         KLIST
     C                   KFLD                    DLCO#
     C                   KFLD                    DLFND
     C*
     C*  KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*  KEY LIST FOR THE REGION
     C*
     C     REKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    DLFND
     C                   KFLD                    EMRGN#
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*** BEGIN ADD *** 05/27/98 ***********************************************
     C*
     C*  KEY LIST FOR SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    ##KEY             2
     C*** END ADD ***** 05/27/98 ***********************************************
     C**** BEGIN ADD **** 03/28/01 ************************************
     C     RPKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C**** END   ADD **** 03/28/01 ************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
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
     C*          *LIKE     DEFN DLSUDT    XXDATE
     C     *LIKE         DEFINE    DLCO#         XXCO#
     C     *LIKE         DEFINE    DLFND         XXFND
     C     *LIKE         DEFINE    DLEMP#        XXEMP#
     C     *LIKE         DEFINE    DLDIV         XXDIV
     C     *LIKE         DEFINE    DODESC        XXDESC
     C     *LIKE         DEFINE    DLFND         HLFND
     C     *LIKE         DEFINE    DLREQ#        HLREQ#
     C     *LIKE         DEFINE    DODEPT        XXDEPT
     C     *LIKE         DEFINE    COST          XXSTAT
     C     *LIKE         DEFINE    COCTY         XXCTY
     C     *LIKE         DEFINE    DLSEQ#        XXSEQ#
     C*** BEGIN DEL *** 04/26/00 **************************************
     C********   *LIKE     DEFN DLSEQ#    ##SEQ#
     C*** END DEL *** 04/26/00 ****************************************
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C*
     C                   MOVE      QQCO#         XXCO#
     C*                    MOVE QQDATE    XXDATE
     C*          XXDATE    MULT 10000.01  XXDATE
     C                   TIME                    DSDTTM
     C*** BEGIN DEL *** 04/26/00 **************************************
     C********             Z-ADD*HIVAL    ##SEQ#
     C*** END DEL *** 04/26/00 ****************************************
     C                   MOVE      'PS'          XXDEPT
     C*
     C*   READ THE WORK FILE RECORD FROM PREVIOUS STEP
     C*
     C                   READ      PDL01                                  60
     C                   Z-ADD     *ZEROS        HLREQ#
     C*
     C*   IF QUOTE SYSTEM INSTALLED, OPEN QUOTE LOGICALS
     C*
     C                   MOVE      DLQTFL        QQCALL            1
     C*
     C*   GET COMPANY INFORMATION FIRST TIME ONLY
     C     *IN60         IFEQ      '0'
     C                   EXSR      INITC                                        INIT CO FIELDS
     C*
     C*   GET COMPANY INFORMATION:
     C     COKEY         CHAIN     PCO01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEA     CONAME        CNB
     C                   Z-ADD     1             X                 3 0
     C     *BLANK        LOOKUP    CNB(X)                             50
     C     *IN50         IFEQ      '1'
     C                   MOVEA     CNB(X)        CN
     C                   ELSE
     C                   MOVEA     CONAME        CN
     C                   END
     C                   MOVEA     CN            DLCNME
     C*
     C* COMPANY ADDRESS
     C                   MOVEL     COADR1        DLCAD1
     C*
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      COST          XXSTAT
     C                   Z-ADD     COZIP         XXZIP
     C                   MOVEL     COCTY         XXCTY
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         DLCCSZ
     C*
     C                   Z-ADD     COFEIN        DLCFID
     C                   END                                                    END 0 *IN50
     C*
     C                   END                                                    END 0 *IN60
     C*
     C**  EACH REQUEST IS READ......
     C     *IN60         DOWEQ     '0'
     C*
     C**  INITIALIZE SEQ# FOR EACH REQUEST (THIS IS THE SEQUENCE COUNTER FOR THE
     C**  WXDLP RECORDS TO KEEP EACH ONE UNIQUE - WILL BE USED IN W4119R)
     C     DLREQ#        IFNE      HLREQ#
     C*
     C*  CHECK TO SEE IF EMPLOYER DETAIL INFO HAS BEEN SET UP FOR
     C*  THIS FUND YEAR/EMPLOYER
     C*
     C                   MOVE      'Y'           XXEMDE            1
     C                   Z-ADD     DLFEMP        XXEMP#
     C                   Z-ADD     DLFDIV        XXDIV
     C     ELKEY         CHAIN     PEL01                              55
     C*
     C     *IN55         IFEQ      '1'
     C                   MOVE      'N'           XXEMDE
     C                   END
     C*
     C                   Z-ADD     DLTEMP        XXEMP#
     C                   Z-ADD     DLTDIV        XXDIV
     C*
     C     ELKEY         CHAIN     PEL01                              55
     C*
     C     *IN55         IFEQ      '1'
     C                   MOVE      'N'           XXEMDE
     C                   END
     C*
     C                   Z-ADD     *ZEROS        XXSEQ#
     C                   Z-ADD     DLREQ#        HLREQ#
     C                   END
     C*
     C**  IF FUND HAS CHANGED, INITIALIZE WORKFIELDS AND GET NEW FUND INFO
     C     DLFND         IFNE      HLFND
     C                   EXSR      INITF
     C                   EXSR      INFOF
     C                   END                                                    END NE HLFND
     C*
     C*   ONLY PROCESS RECORDS WITH EMPLOYER DETAIL
     C*
     C     XXEMDE        IFEQ      'Y'
     C*
     C                   Z-ADD     DLFEMP        XXEMP#
     C                   Z-ADD     DLFDIV        XXDIV
     C*
     C*   CHECK FOR RANGE OF EMPLOYERS - MUST WRITE 1 RECORD FOR EACH
     C     DLFEMP        IFNE      DLTEMP
     C     DLFDIV        ORNE      DLTDIV
     C                   MOVEL     DLTEMP        X1EMP            14 0
     C                   MOVE      DLTDIV        X1EMP
     C*
     C*   MUST STAY WITHIN LIMITS OF RANGE SELECTED
     C     ELKEY         SETLL     PEL01                                        CO/FND/FYR/EMP/DIV
     C     ELKEY1        READE     PEL01                                  65    CO/FND/FYR
     C     *IN65         IFEQ      '0'
     C                   MOVEL     ELEMP#        X2EMP            14 0
     C                   MOVE      ELDIV         X2EMP
     C                   END
     C*
     C     *IN65         DOWEQ     '0'
     C     X2EMP         ANDLE     X1EMP
     C                   Z-ADD     ELEMP#        DLEMP#
     C                   Z-ADD     ELDIV         DLDIV
     C     EMKEY         CHAIN     PEM01                              50        CO/FND/EMP/DIV
     C     *IN50         IFEQ      '0'
     C*
     C*   CHECK FOR ACTIVE EMPLOYERS ONLY SELECTION
     C     DLAEFL        IFEQ      'Y'
     C                   EXSR      ACTIVE                                       ACTIVE EMPLRS ONLY
     C*   ELSE, IF ALL EMPLOYERS TO BE INCLUDED, DISREGARD STATUS
     C                   ELSE                                                   ELSE EQ 'Y'
     C                   EXSR      INIT2                                        INIT ALL OTHER FIELD
     C                   EXSR      WRITE
     C                   EXSR      PRTDOC
     C                   END                                                    END EQ 'Y'
     C*
     C                   END                                                    END 0 *IN50
     C*
     C*   READ NEXT EMPLOYER DETAIL RECORD, MUST BE WITHIN RANGE
     C     ELKEY1        READE     PEL01                                  65
     C     *IN65         IFEQ      '0'
     C                   MOVEL     ELEMP#        X2EMP
     C                   MOVE      ELDIV         X2EMP
     C                   END
     C*
     C*
     C                   END                                                    END DOWEQ 65
     C*
     C                   ELSE                                                   ELS EMP RANG
     C*
     C**  IF ONLY ONE EMPLOYER WAS SELECTED, JUST PROCESS RECORD
     C     ELKEY         CHAIN     PEL01                              65        CO/FND/FYR
     C     *IN65         IFEQ      '0'
     C                   MOVEL     ELEMP#        DLEMP#
     C                   MOVE      ELDIV         DLDIV
     C     EMKEY         CHAIN     PEM01                              50        CO/FND/EMP/DIV
     C     *IN50         IFEQ      '0'
     C*
     C*   CHECK FOR ACTIVE EMPLOYERS ONLY SELECTION
     C     DLAEFL        IFEQ      'Y'
     C                   EXSR      ACTIVE                                       ACTIVE EMPLRS ONLY
     C*   ELSE, IF ALL EMPLOYERS TO BE INCLUDED, DISREGARD STATUS
     C                   ELSE                                                   ELSE EQ 'Y'
     C                   EXSR      INIT2
     C                   EXSR      WRITE
     C                   EXSR      PRTDOC
     C                   END                                                    END EQ 'Y'
     C*
     C                   END                                                    END 0 *IN50
     C                   END                                                    END 0 *IN65
     C*
     C                   END                                                    END EMP RANG
     C                   ELSE                                                   NO EMPL DETAIL
     C                   MOVEL     DLFEMP        X2EMP
     C                   MOVE      DLFDIV        X2EMP
     C                   MOVEL     DLTEMP        X1EMP
     C                   MOVE      DLTDIV        X1EMP
     C                   Z-ADD     DLFEMP        DLEMP#
     C                   Z-ADD     DLFDIV        DLDIV
     C     EMKEY         CHAIN     PEM01                              50
     C     *IN50         DOWEQ     '0'
     C     X2EMP         ANDLE     X1EMP
     C                   Z-ADD     EMEMP#        DLEMP#
     C                   Z-ADD     EMDIV         DLDIV
     C     DLAEFL        IFEQ      'Y'
     C                   EXSR      ACTIVE
     C                   ELSE
     C                   EXSR      INIT2
     C                   EXSR      WRITE
     C                   EXSR      PRTDOC
     C                   END
     C     EMKY2         READE     PEM01                                  50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     EMEMP#        X2EMP
     C                   MOVE      EMDIV         X2EMP
     C                   END                                                    *IN50 = '0'
     C                   END                                                    *IN50 DOW '0'
     C*                                                    & X2EMP <= X1EMP
     C                   END                                                    EMPL DETAIL EXISTS
     C*
     C**  READ NEXT WORKFILE RECORD
     C                   READ      PDL01                                  60
     C*
     C                   END                                                    END DOWEQ 60
     C*
     C*   IF QUOTE SYSTEM INSTALLED, CLOSE QUOTE LOGICALS
     C*
     C*
     C**  END OF PROGRAM
     C*
     C                   MOVE      '1'           *INLR
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      *
      *
      *===============================================================
      * SRCONT - Get Employer Contact Names and phone
      *===============================================================
      *
     C     SRCONT        BEGSR
     C                   CLEAR                   XXCNT
      *
      * Get Contact Name
      *
     C     ECKY          CHAIN     PEC01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     ECFSN         IFNE      *BLANKS
     C     ECFSN         CAT       ECLSN:1       XXCNT            50
      *
     C                   ELSE
      *
     C     ECTITL        CAT       ECLSN:1       XXCNT
     C                   ENDIF
      *
     C                   ENDIF                                                  ECKY
      *
     C                   ENDSR                                                  SRCONT
      *
      *
      ***** END ADD ******* 01/31/02 **********************************
     C*
     C****************************************************
     C*   SUBROUTINE TO EXECUTE PRTDOC COMMAND           *
     C****************************************************
     C     PRTDOC        BEGSR
     C*
******* ADD 5/22/06 ****************************************************
     C****** HERE'S THE TEST FOR MSWORD PROCESSING VS. STD.
     *****      IF THE SYSVAL DEFAULT IS TO USE MSWORD, AND THE USER DEFAULT
     ***         IS MSWORD OR NOT SPECIFIED (*BLANKS) THEN DO THIS, ELSE
     ***         NORMAL PROCESSING SHOULD OCCUR
     **
     C                   MOVE      DLCO#         X@CO#
     C                   CALL      'DA511R'
     C                   PARM                    X@CO#             3
     C                   PARM      'PS'          XXXDPT            2
     C                   PARM                    DLDPCD
     C                   PARM                    DLFLDR
     C                   PARM                    DLDCID
     C                   PARM      *BLANKS       RTN1              1
     C*
     ***USE INTERACTIVE IF SPECIFIED, ELSE NORMAL (BATCH) PROCESSING
     C     RTN1          IFEQ      'I'                                           **INTERACTIVE**
     C                   EXSR      WRDDOC
     C                   GOTO      WASWRD
     C                   ENDIF
******* END ADD 5/22/06 ****************************************************
     C*   BUILD PRTDOC COMMAND
     C*
     C*** Begin chg *** 03/01/01 **************************************
     C                   MOVEA     DLDCID        CMD(15)                        DOCUMENT
     C*******              MOVEADLDCID    CMD,13           DOCUMENT
     C*** End   chg *** 03/01/01 **************************************
     C                   MOVE      DL#CPY        QQ#CPY            5
     C*** Begin chg *** 03/01/01 **************************************
     C                   MOVEA     QQ#CPY        CMD(37)                        COPIES
     C*******              MOVEAQQ#CPY    CMD,35           COPIES
     C*** End   chg *** 03/01/01 **************************************
     C*** 9/1/98 *** 33 WDQ ********NO DARTS IF RMCD IS BLANK ****
     C     DORMCD        IFEQ      *BLANKS
     C*** Begin chg *** 03/01/01 **************************************
     C                   MOVEA     '*NO '        CMD(54)
     C                   ELSE
     C                   MOVEA     '*YES'        CMD(54)
     C*******              MOVEA'*NO '    CMD,52
     C*******              ELSE
     C*******              MOVEA'*YES'    CMD,52
     C*** End   chg *** 03/01/01 **************************************
     C                   ENDIF
     C*** 9/1/98 *** 33 WDQ **END **NO DARTS IF RMCD IS BLANK ****
     C*** Begin chg *** 03/01/01 **************************************
     C                   MOVEA     DLSDOC        CMD(67)                        DARTS DOC
     C*******              MOVEADLSDOC    CMD,65           DARTS DOC
     C*** End   chg *** 03/01/01 **************************************
     C*
     C                   MOVEA     *BLANK        FLR
     C                   MOVEA     DLFLDR        FLR
     C                   Z-ADD     1             I                 3 0
     C*** Begin chg *** 03/01/01 **************************************
     C                   Z-ADD     86            II                3 0
     C*******              Z-ADD84        II      30
     C*** End   chg *** 03/01/01 **************************************
     C*
     C     FLR(I)        DOWNE     *BLANK
     C                   MOVEA     FLR(I)        CMD(II)
     C                   ADD       1             I
     C                   ADD       1             II
     C                   END
     C*
     C                   MOVEA     ''''          CMD(II)
     C                   ADD       1             II
     C*
     C                   MOVEA     ')'           CMD(II)
     C                   ADD       2             II
     C                   MOVEA     'SAVFLR('     CMD(II)
     C*
     C                   ADD       7             II
     C                   MOVEA     ''''          CMD(II)
     C*
     C                   MOVEA     *BLANK        FLR
     C*** 9/1/98 *** 33 WDQ **ADD **NO DARTS IF RMCD IS BLANK ****
     C     DORMCD        IFNE      *BLANKS
     C                   MOVEA     DLSFLD        FLR
     C                   ELSE
     C                   MOVEA     '*BLANK '     FLR
     C*** 9/1/98 *** 33 WDQ **ADD **NO DARTS IF RMCD IS BLANK ****
     C                   ENDIF
     C******************** MOVEADLSFLD    FLR
     C                   ADD       1             II
     C                   Z-ADD     1             I
     C     FLR(I)        DOWNE     *BLANK
     C                   MOVEA     FLR(I)        CMD(II)
     C                   ADD       1             I
     C                   ADD       1             II
     C                   END
     C*
     C                   MOVEA     ''')'         CMD(II)
     C*
     C*   SPECIFY THE SELECTED OUTPUT QUEUE ON THE PRTDOC COMMAND
     C*
     C                   ADD       3             II
     C                   MOVEA     'OUTQ('       CMD(II)
     C                   ADD       5             II
     C                   MOVEA     DLOUTQ        CMD(II)
     C                   ADD       10            II
     C                   MOVEA     ')'           CMD(II)
     C*
      **** BEGIN ADD **** 01/26/04 *********************************************
     C                   ADD       2             II
     C                   MOVEA     'JOBQ'        CMD(II)
     C                   ADD       4             II
     C                   MOVEA     '(*NO)'       CMD(II)
      **** END   ADD **** 01/26/04 *********************************************
     C*
     C                   MOVEA     CMD           CMDSTR          225
     C                   Z-ADD     225           LENGTH           15 5
     C*
     C                   CALL      'QCMDEXC'                            37
     C                   PARM                    CMDSTR
     C                   PARM                    LENGTH
     C*
     C*** BEGIN ADD *** 05/28/98 **************************************
 **********  ADD 5/22/06 **********************************************
     **  BYPASSED THE PRTDOC
     C     WASWRD        TAG
 ********* END ADD 5/22/06 *******************************************
     C*
     C*  ONLY BUILD CHGDOCD COMMAND IF CHANGES ARE NOT TO BE ALLOWED
     C*  TO THE RESOLVED DOCUMENT.
     C*
     C     DOKEY         CHAIN     PDO01                              50
     C     *IN50         IFEQ      '0'
     C*
     C     DOALCH        IFEQ      'N'
     C     DORMCD        ANDNE     *BLANKS                                       ADD 9/10/98 #02
     C*** END ADD ***** 05/28/98 **************************************
     C*
     C*   BUILD CHGDOCD COMMAND
     C*
     C*** Begin chg *** 03/01/01 **************************************
     C                   MOVEA     DLSDOC        CM2(16)                        SAVE DOCUMENT
     C*******              MOVEADLSDOC    CM2,14           SAVE DOCUMENT
     C*** End   chg *** 03/01/01 **************************************
     C*
     C                   MOVEA     *BLANK        FLR
     C                   MOVEA     DLSFLD        FLR
     C                   Z-ADD     1             I
     C*** Begin chg *** 03/01/01 **************************************
     C                   Z-ADD     48            II
     C*******              Z-ADD46        II
     C*** End   chg *** 03/01/01 **************************************
     C*
     C     FLR(I)        DOWNE     *BLANK                                       SAVE FOLDER
     C                   MOVEA     FLR(I)        CM2(II)
     C                   ADD       1             I
     C                   ADD       1             II
     C                   ENDDO
     C*
     C                   MOVEA     ''''          CM2(II)
     C                   ADD       1             II
     C                   MOVEA     ')'           CM2(II)
     C*
     C                   MOVEA     CM2           CM2STR          225
     C*
     C                   CALL      'QCMDEXC'                            37
     C                   PARM                    CM2STR
     C                   PARM                    LENGTH
     C*
     C*** BEGIN ADD *** 05/28/98 **************************************
     C                   ENDIF                                                  DOALCH = 'N'
     C*
     C                   ENDIF                                                  *IN50 = '0'
     C*** END ADD ***** 05/28/98 **************************************
     C*
     C*   PRTDOC COMMAND USED WITH INDIVIDUAL EMPLOYER.  AFTER COMPLETION,
     C*   DELETE WXDLL RECORD BECAUSE OF POSSIBILITY OF OTHER EMPLOYERS IN
     C*   USER REQUEST.
     C*
     C     DLKEY         DELETE    PDL03                              61
     C     DLKEY         DELETE    PD201                              61
     C*
     C                   ENDSR
     C* **************************************************
     C*   SUBROUTINE TO DETERMINE IF EMPLOYER IS ACTIVE  *
     C* **************************************************
     C     ACTIVE        BEGSR
     C*
     C     XXEMDE        IFEQ      'Y'
     C     EMKEY         CHAIN     PEM01                              50        CO/FND/EMP/DIV
     C                   END                                                    XXEMDE = 'Y'
     C     *IN50         IFEQ      '0'
     C*
     C                   Z-ADD     1             Y                 1 0
     C     *ZERO         LOOKUP    ED(Y)                                  50
     C     *IN50         IFEQ      '0'
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   END
     C     XXEMDE        IFEQ      'Y'
     C     Y             DOWNE     0
     C     ELPTDT        COMP      ED(Y)                              50  50
     C  N50              SUB       1             Y
     C  N50              END                                                    DLFYR <> *ZEROS
     C                   END                                                    XXEMDE = 'Y'
     C*
     C     Y             IFNE      *ZERO
     C                   EXSR      INIT2                                        INIT ALL OTHER FIELD
     C                   EXSR      WRITE
     C                   EXSR      PRTDOC
     C                   END                                                    END NE *ZERO
     C                   END                                                    END 0  *IN50
     C*
     C                   ENDSR
     C*******************************************************
     C*   SUBROUTINE TO INITIALIZE COMPANY WORKFILE FIELDS  *
     C*******************************************************
     C     INITC         BEGSR
     C*
     C* COMPANY INFORMATION
     C                   MOVE      *BLANKS       DLCNME
     C                   MOVE      *BLANKS       DLCAD1
     C                   MOVE      *BLANKS       DLCAD2
     C                   MOVE      *BLANKS       DLCCSZ
     C                   Z-ADD     *ZEROS        DLCFID
     C*
     C                   ENDSR
     C****************************************************
     C*   SUBROUTINE TO INITIALIZE FUND WORKFILE FIELDS  *
     C****************************************************
     C     INITF         BEGSR
     C*
     C* FUND INFORMATION
     C                   MOVE      *BLANKS       DLFNME
     C                   MOVE      *BLANKS       DLFSHN
     C                   MOVE      *BLANKS       DLFMA1
     C                   MOVE      *BLANKS       DLFMA2
     C                   MOVE      *BLANKS       DLFMCS
     C                   MOVE      *BLANKS       DLFPA1
     C                   MOVE      *BLANKS       DLFPA2
     C                   MOVE      *BLANKS       DLFPCS
     C                   Z-ADD     *ZEROS        DLFPH1
     C                   Z-ADD     *ZEROS        DLFARE
     C                   Z-ADD     *ZEROS        DLFPH2
     C                   MOVE      *BLANKS       DLFUN1
     C                   MOVE      *BLANKS       DLFUN2
     C                   MOVE      *BLANKS       DLFUN3
     C                   MOVE      *BLANKS       DLFUN4
     C                   MOVE      *BLANKS       DLFUN5
     C                   MOVE      *BLANKS       DLFUN6
     C                   MOVE      *BLANKS       DLFUN7
     C*
     C                   Z-ADD     DLFND         HLFND
     C*
     C                   ENDSR
     C****************************************************
     C*   SUBROUTINE TO GET FUND INFORMATION             *
     C****************************************************
     C     INFOF         BEGSR
     C*
     C*   GET FUND INFORMATION:
     C     FNKEY         CHAIN     PFN01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEA     FNNAME        FNB
     C                   Z-ADD     1             X                 3 0
     C     *BLANK        LOOKUP    FNB(X)                             50
     C     *IN50         IFEQ      '1'
     C                   MOVEA     FNB(X)        FN
     C                   ELSE
     C                   MOVEA     FNNAME        FN
     C                   END
     C                   MOVEA     FN            DLFNME
     C                   MOVEL     FNSHNM        DLFSHN
     C*
     C* FUND MAILING ADDRESS
     C                   MOVEL     FNMAD1        DLFMA1
     C                   MOVEL     FNMAD2        DLFMA2
     C*
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      FNMST         XXSTAT
     C                   Z-ADD     FNMZIP        XXZIP
     C                   MOVEL     FNMCTY        XXCTY
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         DLFMCS
     C*
     C* FUND PHYSICAL ADDRESS
     C                   MOVEL     FNPAD1        DLFPA1
     C                   MOVEL     FNPAD2        DLFPA2
     C*
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      FNPST         XXSTAT
     C                   Z-ADD     FNPZIP        XXZIP
     C                   MOVEL     FNPCTY        XXCTY
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         DLFPCS
     C*
     C* FUND PHONE NUMBERS
     C                   Z-ADD     FNPH1A        DLFP1A
     C                   Z-ADD     FNPH1         DLFPH1
     C                   Z-ADD     FNAREA        DLFARE
     C                   Z-ADD     FNPH2         DLFPH2
     C                   Z-ADD     FNFAXA        DLFFXA
     C                   Z-ADD     FNFAX         DLFFAX
     C*
     C                   MOVEL     FNANAM        DLANAM
     C                   MOVEL     FNAAD1        DLAAD1
     C                   MOVEL     FNAAD2        DLAAD2
     C                   MOVEL     FNACTY        DLACTY
     C                   MOVEL     FNAST         DLAST
     C                   MOVEL     FNAZIP        DLAZIP
     C                   MOVEL     FNIADR        DLFIAD
     C                   END                                                    END 0 *IN50
     C*
     C*   GET FUND AUXILIARY INFORMATION:
     C     F2KEY         CHAIN     PF201                              50
     C     *IN50         IFEQ      '0'
     C                   MOVE      F2UNM1        DLFUN1
     C                   MOVE      F2UNM2        DLFUN2
     C                   MOVE      F2UNM3        DLFUN3
     C                   MOVE      F2UNM4        DLFUN4
     C                   MOVE      F2UNM5        DLFUN5
     C                   MOVE      F2UNM6        DLFUN6
     C                   MOVE      F2UNM7        DLFUN7
     C                   END                                                    END 0 *IN50
     C*
     C                   ENDSR
     C****************************************************
     C*   SUBROUTINE TO INITIALIZE WORKFILE FIELDS    2  *
     C****************************************************
     C     INIT2         BEGSR
     C*
     C* EMPLOYER INFORMATION
     C                   MOVE      *BLANKS       DLENME
     C                   MOVE      *BLANKS       DLEMA1
     C                   MOVE      *BLANKS       DLEMA2
     C                   MOVE      *BLANKS       DLEMCS
     C                   MOVE      *BLANKS       DLEPA1
     C                   MOVE      *BLANKS       DLEPA2
     C                   MOVE      *BLANKS       DLEPCS
     C                   Z-ADD     *ZEROS        DLEARE
     C                   Z-ADD     *ZEROS        DLEPH#
     C                   MOVE      *BLANKS       DLERGN
     C                   Z-ADD     *ZEROS        DLEFID
     C                   MOVE      *BLANKS       DLECNT
     C                   MOVE      *BLANKS       DLEATT
     C                   MOVE      *BLANKS       DLEDBA
     C                   MOVE      *BLANKS       DLECM1
     C                   MOVE      *BLANKS       DLECM2
     C                   MOVE      *BLANKS       DLECM3
     C                   MOVE      *BLANKS       DLESEM
     C                   MOVE      *BLANKS       DLECTY
     C                   MOVE      *BLANKS       DLECHP
     C                   MOVE      *BLANKS       DLUWNM                         ADD 06/26/91
      ***** BEG ADD ******* 11/18/02 **********************************
     C                   MOVEL(P)  *BLANKS       DLUWEM
      ***** END ADD ******* 11/18/02 **********************************
     C                   Z-ADD     *ZEROS        DLDT5
     C                   MOVE      *BLANKS       DLSUNM
     C                   MOVE      *BLANKS       DLCENM
     C                   MOVE      *BLANKS       DLFANM
     C                   MOVE      *BLANKS       DLUMNM
     C*
     C* EMPLOYER DETAIL INFORMATION
     C                   Z-ADD     *ZEROS        DLEEAP
     C                   Z-ADD     *ZEROS        DLEPFD
     C                   Z-ADD     *ZEROS        DLEPTD
     C                   Z-ADD     *ZEROS        DLEPRF
     C*
     C* AGENT MASTER INFORMATION
     C                   MOVE      *BLANKS       DLANME
     C                   MOVE      *BLANKS       DLAMA1
     C                   MOVE      *BLANKS       DLAMCS
     C                   MOVE      *BLANKS       DLAPA1
     C                   MOVE      *BLANKS       DLAPCS
     C                   Z-ADD     *ZEROS        DLAAR1
     C                   Z-ADD     *ZEROS        DLAPH1
     C                   Z-ADD     *ZEROS        DLAAR2
     C                   Z-ADD     *ZEROS        DLAPH2
     C                   MOVE      *BLANKS       DLASEC
     C                   MOVE      *BLANKS       DLACNT
     C                   MOVE      *BLANKS       DLACM1
     C                   MOVE      *BLANKS       DLACM2
     C*
     C* AGENT ASSIGNMENT INFORMATION
     C                   Z-ADD     *ZEROS        DLAFID
     C                   Z-ADD     *ZEROS        DLAMOD
     C*
     C* AGENT HISTORY INFORMATION
     C                   Z-ADD     *ZEROS        DLAAGT
     C                   Z-ADD     *ZEROS        DLAEFD
     C                   Z-ADD     *ZEROS        DLAPRO
     C                   MOVE      *BLANKS       DLAPNM
     C*
     C* FOLDER/DOCUMENT INFORMATION
     C                   MOVE      *BLANKS       DLSFLD
     C                   MOVE      *BLANKS       DLSDOC
     C                   Z-ADD     *ZERO         DLEFXA
     C                   Z-ADD     *ZERO         DLEFAX
      *
     C                   MOVEL     *BLANK        DLEPSC
     C                   MOVEL     *BLANK        DLEPSM
      ***** BEG ADD ******* 11/18/02 **********************************
     C                   MOVEL(P)  *BLANKS       DLEPEM
      ***** END ADD ******* 11/18/02 **********************************
     C                   Z-ADD     *ZERO         DLEPSX
      *
     C                   MOVEL     *BLANK        DLUWNC
     C                   MOVEL     *BLANK        DLUWNM
     C                   Z-ADD     *ZERO         DLUWNX
      *
     C                   MOVEL     *BLANK        DLPACC
     C                   MOVEL     *BLANK        DLPACN
     C                   Z-ADD     *ZERO         DLPACX
      *
     C                   MOVEL     *BLANK        DLARVW
     C                   MOVEL     *BLANK        DLARVN
     C                   Z-ADD     *ZERO         DLARVX
      *
     C                   MOVEL     *BLANK        DLAMGR
     C                   MOVEL     *BLANK        DLAMGN
     C                   Z-ADD     *ZERO         DLAMGX
      *
     C                   MOVEL     *BLANK        DLLREP
     C                   MOVEL     *BLANK        DLLREN
     C                   Z-ADD     *ZERO         DLLREX
      *
      *  ACTIVITY STATUS, DESC
      *
     C                   MOVEL     *BLANK        DLEAST
     C                   MOVEL(P)  *BLANK        DLEASD
      *
     C                   MOVEL     *BLANK        DLERGN
     C                   MOVEL(P)  *BLANK        DLERGD
      *
     C                   MOVEL     *BLANK        DLECTY
     C                   MOVEL(P)  *BLANK        XXKEY
     C                   MOVEL(P)  *BLANK        DLECTD
     C                   MOVEL     *BLANK        DLECHP
     C                   MOVEL(P)  *BLANK        DLECHD
     C                   MOVEL     *BLANK        DLSIC
     C                   MOVEL(P)  *BLANK        DLSICD
     C                   Z-ADD     *ZERO         DLEFDT
     C                   Z-ADD     *ZERO         DLCADT
      *
     C                   Z-ADD     *ZERO         DLEFID
     C                   MOVEL     *BLANK        DLECNT
     C                   MOVEL     *BLANK        DLEATT
     C                   MOVEL     *BLANK        DLECN3
     C                   MOVEL     *BLANK        DLEDBA
     C                   MOVEL     *BLANK        DLECM1
     C                   MOVEL     *BLANK        DLECM2
     C                   MOVEL     *BLANK        DLECM3
     C                   MOVEL     *BLANK        DLESEM
     C                   MOVEL     *BLANK        DLERSK
     C**
     C                   MOVEL     *BLANK        DLEBUS
     C                   MOVEL     *BLANK        DLEBUD
     C                   MOVEL     *BLANK        DLENOB
     C                   MOVEL     *BLANK        DLIADR
     C                   MOVEL     *BLANK        DLSIC
     C                   MOVEL(P)  *BLANK        DLSICD
      ***** BEG ADD ******* 11/18/02 **********************************
      *
     C                   MOVEL(P)  *BLANKS       DLUPEM
     C                   MOVEL(P)  *BLANKS       DLEMEM
     C                   MOVEL(P)  *BLANKS       DLAGEM
     C                   MOVEL(P)  *BLANKS       DLAPEM
     C                   MOVEL(P)  *BLANKS       DLC1EM
     C                   MOVEL(P)  *BLANKS       DLC2EM
     C                   MOVEL(P)  *BLANKS       DLC3EM
     C                   MOVEL(P)  *BLANKS       DLEPEM
     C                   MOVEL(P)  *BLANKS       DLUWEM
     C                   MOVEL(P)  *BLANKS       DLPAEM
     C                   MOVEL(P)  *BLANKS       DLAREM
     C                   MOVEL(P)  *BLANKS       DLAMEM
     C                   MOVEL(P)  *BLANKS       DLLREM
      ***** END ADD ******* 11/18/02 **********************************
      *
     C                   ENDSR
      *
      *
      *
     C****************************************************
     C*   SUBROUTINE TO LOAD THE CITY STATE & ZIP        *
     C****************************************************
     C     SRADDR        BEGSR
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     XXSTAT        XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ            40
     C                   ENDSR
     C****************************************************
     C*   SUBROUTINE TO WRITE RECORDS                    *
     C****************************************************
     C     WRITE         BEGSR
     C*
     C*** BEGIN ADD *** 05/28/98 **************************************
     C     DOKEY         CHAIN     PDO01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVE      *BLANKS       XXDESC
     C                   MOVEL     DODESC        XXDESC
     C                   END
     C*** END ADD ***** 05/28/98 **************************************
     C*
     C**  WRITE RECORD TO DARTS ******************************************
     C*
     C**  ESTABLISH KEY
     C*** BEGIN DEL *** 04/26/00 **************************************
     C********             MOVE *BLANKS   CMKEY
     C********             MOVE *BLANKS   QCKEY
     C********             MOVE XXCO#     QCCO#
     C********             MOVE DLFND     QCFND
     C********             MOVE DLEMP#    QCEMP#
     C********             MOVE DLDIV     QCDIV
     C********             MOVE DLQTFL    QCQTFL
     C****ESTABLISH RECORD
     C********             MOVELQCKEY     CMKEY
     C********             MOVE *BLANKS   CMCSID
     C********             MOVELXXDESC    CMCOMM
     C********             MOVE DLDCID    CMCOMM
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMCDAT
     C********             MOVELDORMTY    CMCODE
     C********             MOVELDORMCD    CMRMCD
     C********
     C****GET*FOLDER NAME
     C*** END DEL *** 04/26/00 ****************************************
     C****BEGIN DEL *** 05/27/98 ***********************************************
     C********             MOVE *BLANKS   QQFND   8
     C********             MOVE *BLANKS   QQSER   8
     C********             MOVE *BLANKS   QQSUB   8
     C********             MOVE *BLANKS   QQFOLD  8
     C********             MOVE *BLANKS   QQFLDX  7
     C********             MOVE *BLANKS   QQFPTH 40
     C********             MOVE *BLANKS   QQTERM  1
     C********
     C********             MOVELDLFND     QQFND            for Fund
     C********             MOVELDOASRV    QQSER            for Area of Service
     C********             MOVELDOSUBA    QQSUB            for Sub-Area
     C********
     C********             MOVEL'M'       QQFOLD           means "Member"
     C********             MOVE DLEMP#    QQFLDX           EMP# for Folder name
     C********             MOVE QQFLDX    QQFOLD
     C********
     C********             CALL 'SRFBLD'
     C********             PARM           QQFND
     C********             PARM           QQSER
     C********             PARM           QQSUB
     C********             PARM           QQFOLD
     C********             PARM           QQFPTH           Built, returned
     C********             PARM           QQTERM
     C********
     C********             MOVELQQFPTH    CMFOLD
     C********             MOVELQQFPTH    DLSFLD
     C********
     C**  GET DOCUMENT ID
     C********             MOVE *BLANKS   QQRT    2
     C********             MOVE *BLANKS   QQDOC  12
     C********             MOVE *BLANKS   QQTERM  1
     C********
     C********             MOVELDORMTY    QQRT             Remark Type
     C********             CALL 'SRDBLD'
     C********             PARM           QQFPTH           From SRFBLD routine
     C********             PARM           QQRT             Rmrk Type for Doc ID
     C********             PARM           QQDOC            Built, returned
     C********             PARM           QQTERM
     C********
     C********             MOVELQQDOC     CMDCID
     C********             MOVELQQDOC     DLSDOC
     C*** END DEL ***** 05/27/98 ***********************************************
     C*** BEGIN ADD *** 05/27/98 ***********************************************
     C*** BEGIN DEL *** 04/26/00 **************************************
     C********
     C***DETERMINE IF CLIENT IS ENROLLED IN "OV/400"
     C********
     C********             MOVEL'SY'      ##KEY
     C********   SYKY      CHAINSSY01                60
     C********   *IN60     IFEQ *OFF
     C********
     C***GET*DOC ID AND FOLDER NAME FOR RESOLUTION OF DOCUMENT TO OV/400
     C********
     C********             MOVE DLCO#     Q2CO#
     C********             MOVE DLFND     Q2FND
     C****ADD*9/1/98 *** 33 WDQ *** NO DOC IF DORMCD IS BLANKS ***
     C********   DORMCD    IFNE *BLANKS
     C********             CALL 'SNXTDCID'
     C********             PARM *BLANKS   Q2FLR  80
     C********             PARM *BLANKS   Q2DCID 12
     C********             PARM 'DL'      Q2PRE   2
     C********             PARM           Q2CO#   3
     C********             PARM           Q2FND   3
     C********             PARM *BLANKS   Q2RTCD  1
     C********             MOVELQ2FLR     CMFOLD
     C********             MOVELQ2FLR     DLSFLD
     C********             MOVELQ2DCID    CMDCID
     C********             MOVELQ2DCID    DLSDOC
     C****ADD*9/1/98 *** 33 WDQ *** NO DOC IF DORMCD IS BLANKS ***
     C********             ENDIF
     C********             ENDIF
     C****END*ADD ***** 05/27/98 ***********************************************
     C********
     C***W0050R IS KEYED PGM USED FOR SMKDP KEY DEFINITION FILE.
     C********             MOVEL'W0050R'  CMPNAM           KEYED PGM
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMTRDT
     C********             Z-ADDDSTIME    CMTRTM
     C********             MOVE *ZEROS    CMSECL
     C********             MOVE 'N'       CMSBYL
     C********             MOVE *BLANKS   CMSTAT
     C********             MOVELDLIUSR    CMUSER
     C********             MOVEL'W4117R'  CMPGM
     C********             Z-ADDDSTIME    CMTIME
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMDT
     C********             MOVELDLIUSR    CMIUSR
     C********             MOVE *ZEROS    CMDCDT
     C****BEGIN DEL *** 05/13/98 ***********************************************
     C********             MOVE *BLANKS   CMDIFL
     C****END*DEL ***** 05/13/98 ***********************************************
     C********
     C********             MOVE 'WC'      CMAPID
     C********             MOVE *BLANKS   CMIDOC
     C********             MOVE *BLANKS   CMIQUE
     C********             MOVE *BLANKS   CMIMGF
     C********             MOVE *BLANKS   CMIPTH
     C********             MOVE *ZEROS    CMPIDX
     C********             MOVE *BLANKS   CMQPTY
     C********             MOVE *BLANKS   CMDCTP
     C********             MOVEL'W4117R'  CMIPGM
     C********                                                         *********
     C****BEGIN DEL *** 02/23/98 ***********************************************
     C**********           MOVE *BLANKS   CMSMCD
     C****END*  DEL *** 02/23/98 ***********************************************
     C********                                                         *********
     C****BEGIN ADD *** 02/23/98 ***********************************************
     C********                                                         *********
     C********   CMRMCD    CHAINPCT01                60
     C********                                                         *********
     C********   *IN60     IFEQ *OFF
     C********             MOVE CTSMCD    CMSMCD
     C****BEGIN ADD *** 05/13/98 ***********************************************
     C********             MOVE CTFL01    CMDIFL
     C****END*ADD ***** 05/13/98 ***********************************************
     C********             ENDIF                           *IN60=OFF
     C********                                                         *********
     C****END*  ADD *** 02/23/98 ***********************************************
     C********
     C********             MOVE *BLANKS   CMFL01
     C********             MOVE *BLANKS   CMFL02
     C********             MOVE *BLANKS   CMFL03
     C********             MOVE *BLANKS   CMFL04
     C********             MOVE *BLANKS   CMFL05
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMIDT
     C********
     C****GET*LAST SEQ# USED
     C********
     C********   C2KY      SETGTPCM0I
     C********             READPPCM0I                    60
     C********   *IN60     IFEQ '0'
     C********   QCKEY     ANDEQPIKEY
     C********   PISEQ#    ADD  1         CMSEQ#
     C********             ELSE
     C********             Z-ADD0         CMSEQ#
     C********             END
     C****ADD* 9/1/98 *** DONT WRITE DARTS IF RMCD IS BLANKS
     C********   DORMCD    IFNE *BLANKS
     C********             WRITEPCM01
     C******BEGIN ADD **** 07/01/99 ***********************************
     C********             FEOD SDCML
     C******END   ADD **** 07/01/99 ***********************************
     C********             ENDIF
     C*** END DEL *** 04/26/00 ****************************************
     C*** BEGIN ADD *** 04/26/00 **************************************
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      DLFND         QCFND
     C                   MOVE      DLEMP#        QCEMP#
     C                   MOVE      DLDIV         QCDIV
     C                   MOVE      DLQTFL        QCQTFL
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   MOVEL(P)  XXDESC        Q#COMM          256
     C                   MOVE      DLDCID        Q#COMM
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  DORMCD        Q#RMCD            5
     C                   MOVEL     'SY'          ##KEY
     C     SYKY          CHAIN     SSY01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      DLCO#         Q2CO#
     C                   MOVE      DLFND         Q2FND
     C     DORMCD        IFNE      *BLANKS
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       Q2FLR            80
     C                   PARM      *BLANKS       Q2DCID           12
     C                   PARM      'DL'          Q2PRE             2
     C                   PARM                    Q2CO#             3
     C                   PARM                    Q2FND             3
     C                   PARM      *BLANKS       Q2RTCD            1
     C                   MOVEL(P)  Q2FLR         Q#FOLD           80
     C                   MOVEL     Q2FLR         DLSFLD
     C                   MOVEL(P)  Q2DCID        Q#DCID           12
     C                   MOVEL     Q2DCID        DLSDOC
     C                   ENDIF                                                  *IN60 = Off
     C                   ENDIF                                                  RMCD <> blanks
     C                   MOVEL(P)  'W0050R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  DLIUSR        Q#USER           10
     C                   MOVEL(P)  'W4117R'      Q#PGM            10
     C     Q#RMCD        CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CTFL01        Q#DIFL            1
     C                   ENDIF                                                  *IN60 = Off
     C                   MOVEL(P)  'W4117R'      Q#IPGM           10
     C                   MOVEL(P)  *BLANKS       Q#FL01            1
     C                   MOVEL(P)  *BLANKS       Q#FL02            1
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  'WC'          Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANKS       Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANKS       Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     DORMTY        Q#CODE            2
     C     Q#RMCD        IFNE      *BLANKS
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
     C*** begin add ***  07/10/14  **********************************************
     C                   Eval      x#RRN = dbRRN
     C                   Move      q#Seq#        x#Seq#
     C
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W7413RDL'    XXPGNM           10
     C                   PARM      *BLANKS       XXRTSM            1
      /free

       //* Only generate records if called from policy page cover letter program.
       If xxRTSM = *on;

          Chain keySDCMLA pcm0a;

          If %found;
             x#Dcid = CMDcid;
             x#Fold = CMFold;
             x#Uncn = CMUncn;

             GenerateNotice(x#RRN:x#Mode:x#Type:x#Key:x#Dcid:x#Fold:x#Seq#:
              x#Uncn);
          Endif;

       Endif;

      /end-free
     C*** end add *****  07/10/14  **********************************************
     C                   ENDIF                                                  RMCD <> blanks
     C*** END ADD *** 04/26/00 ****************************************
     C*
     C**  WRITE RECORD TO WXDLP ******************************************
     C     XXEMDE        IFEQ      'Y'
     C*
     C*   GET POLICY LETTERS AUX FILE
     C                   MOVE      *BLANK        XXD2P             1
     C     DLKEY         CHAIN     PD201                              66
     C     *IN66         IFEQ      *OFF
     C                   MOVEL     'Y'           XXD2P
     C                   END
     C*
     C*   GET EMPLOYER DETAIL INFORMATION:
     C                   Z-ADD     ELANPR        DLEEAP
     C*
     C*   CONVERT ELPFDT FROM YYMMDD TO MMDDYY
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DLEPFD
     C*
     C*   CONVERT ELPTDT FROM YYMMDD TO MMDDYY
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DLEPTD
     C**** BEGIN ADD **** 03/28/01 ************************************
     C     RPKY          CHAIN     PRP01                              60
     C                   MOVEL     RPCAP#        DLCAP#
     C**** END   ADD **** 03/28/01 ************************************
     C*
     C                   Z-ADD     ELPROR        DLEPRF
     C                   ELSE                                                   NO EMPL DETAIL
     C                   Z-ADD     *ZEROS        DLEEAP
     C                   Z-ADD     *ZEROS        DLEPFD
     C                   Z-ADD     *ZEROS        DLEPTD
     C                   Z-ADD     *ZEROS        DLEPRF
     C                   END                                                    EMPL DETAIL EXISTS
     C*
     C*  GET EMPLOYER INFORMATION:
     C                   MOVEL     EMNAME        DLENME
     C                   MOVEL     EMFDNM        DLEDBA
     C*
     C*  EMPLOYER MAILING ADDRESS
     C                   MOVEL     EMMAD1        DLEMA1
     C                   MOVEL     EMMAD2        DLEMA2
     C*
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      EMMST         XXSTAT
     C                   Z-ADD     EMMZIP        XXZIP
     C                   MOVEL     EMMCTY        XXCTY
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         DLEMCS
     C*
     C*  EMPLOYER PHYSICAL ADDRESS
     C                   MOVEL     EMPAD1        DLEPA1
     C                   MOVEL     EMPAD2        DLEPA2
     C*
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      EMPST         XXSTAT
     C                   Z-ADD     EMPZIP        XXZIP
     C                   MOVEL     EMPCTY        XXCTY
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         DLEPCS
     C*
     C*  EMPLOYER ADDITIONAL INFORMATION
     C                   Z-ADD     EMAREA        DLEARE
     C                   Z-ADD     EMPHN#        DLEPH#
     C                   Z-ADD     EMFAXA        DLEFXA
     C                   Z-ADD     EMFAX#        DLEFAX
      *
     C                   MOVEL     EMPSRV        DLEPSC
     C     EMPSRV        CHAIN     PUP01                              50
     C  N50              MOVEL     UPUSNM        DLEPSM
     C  N50              Z-ADD     UPEXT#        DLEPSX
      ***** BEG ADD ******* 11/18/02 **********************************
     C  N50              MOVEL(P)  UPMAIL        DLEPEM
     C   50              MOVEL(P)  *BLANKS       DLEPEM
      ***** END ADD ******* 11/18/02 **********************************
      *
     C                   MOVEL     EMUWTR        DLUWNC
     C     EMUWTR        CHAIN     PUP01                              50
     C  N50              MOVEL     UPUSNM        DLUWNM
     C  N50              Z-ADD     UPEXT#        DLUWNX
      ***** BEG ADD ******* 11/18/02 **********************************
     C  N50              MOVEL(P)  UPMAIL        DLUWEM
     C   50              MOVEL(P)  *BLANKS       DLUWEM
      ***** END ADD ******* 11/18/02 **********************************
      *
     C                   MOVEL     EMPACC        DLPACC
     C     EMPACC        CHAIN     PUP01                              50
     C  N50              MOVEL     UPUSNM        DLPACN
     C  N50              Z-ADD     UPEXT#        DLPACX
      ***** BEG ADD ******* 11/18/02 **********************************
     C  N50              MOVEL(P)  UPMAIL        DLPAEM
     C   50              MOVEL(P)  *BLANKS       DLPAEM
      ***** END ADD ******* 11/18/02 **********************************
      *
     C                   MOVEL     EMARVW        DLARVW
     C     EMARVW        CHAIN     PUP01                              50
     C  N50              MOVEL     UPUSNM        DLARVN
     C  N50              Z-ADD     UPEXT#        DLARVX
      ***** BEG ADD ******* 11/18/02 **********************************
     C  N50              MOVEL(P)  UPMAIL        DLAREM
     C   50              MOVEL(P)  *BLANKS       DLAREM
      ***** END ADD ******* 11/18/02 **********************************
      *
     C                   MOVEL     EMAMGR        DLAMGR
     C     EMAMGR        CHAIN     PUP01                              50
     C  N50              MOVEL     UPUSNM        DLAMGN
     C  N50              Z-ADD     UPEXT#        DLAMGX
      ***** BEG ADD ******* 11/18/02 **********************************
     C  N50              MOVEL(P)  UPMAIL        DLAMEM
     C   50              MOVEL(P)  *BLANKS       DLAMEM
      ***** END ADD ******* 11/18/02 **********************************
      *
     C                   MOVEL     EMLREP        DLLREP
     C     EMLREP        CHAIN     PUP01                              50
     C  N50              MOVEL     UPUSNM        DLLREN
     C  N50              Z-ADD     UPEXT#        DLLREX
      ***** BEG ADD ******* 11/18/02 **********************************
     C  N50              MOVEL(P)  UPMAIL        DLLREM
     C   50              MOVEL(P)  *BLANKS       DLLREM
      ***** END ADD ******* 11/18/02 **********************************
      *
      *  ACTIVITY STATUS, DESC
      *
     C***** BEG ADD ******* 07/07/05 **********************************
     C                   MOVEL(P)  ELASCD        EMASTS
     C***** END ADD ******* 07/07/05 **********************************
     C                   MOVEL     EMASTS        DLEAST
     C     EMASTS        IFNE      *BLANK
     C                   MOVEL     EMASTS        XXKEY
     C     TBKY          CHAIN     PAV01                              60
     C  N60              MOVEL(P)  AVDESC        DLEASD
     C                   END
      *
     C*                    MOVEL'ACST'    XXTBID    P
     C*                    MOVELEMASTS    XXKEY     P
     C*          GTKY      CHAINPGT01                60
     C*          *IN60     IFEQ *OFF
     C*          GTSTAT    ANDEQ'A'
     C*                    MOVELGTDESC    DLEASD    P
     C*                    END
      *
     C                   MOVEL     EMRGN#        DLERGN
     C     REKY          CHAIN     PRE01                              60
     C  N60              MOVEL(P)  REDESC        DLERGD
      *
     C                   MOVEL     EMCNTY        DLECTY
     C*                    MOVELEMCNTY    XXKEY     P
     C*
     C     EMPST         IFNE      *BLANK
     C     EMPST         CAT       EMCNTY:0      XXKEY
     C                   ELSE
     C     EMMST         CAT       EMCNTY:0      XXKEY
     C                   ENDIF                                                  SCPST<>*BLANKS
     C*
     C     TBKY          CHAIN     PCN01                              60
     C  N60              MOVEL(P)  CNDESC        DLECTD
      *
     C                   MOVEL     EMCHAP        DLECHP
     C     CPKY          CHAIN     PCP01                              60
     C  N60              MOVEL(P)  CPDESC        DLECHD
      *
     C                   MOVEL     EMSIC         DLSIC
     C                   MOVEL     EMSIC         XXKEY
     C*** BEG CHG *** 08/15/00 ****************************
     C                   MOVEL(P)  'SIC'         XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        DLSICD
     C                   ENDIF
     C********** TBKY      CHAINPSC01                60
     C**N60*****           MOVELSCDESC    DLSICD    P
     C*** END CHG *** 08/15/00 ****************************
      *
     C                   SELECT
     C     EMEFD5        WHENNE    *ZERO
     C                   Z-ADD     EMEFD5        DLEFDT
     C     EMEFD4        WHENNE    *ZERO
     C                   Z-ADD     EMEFD4        DLEFDT
     C     EMEFD3        WHENNE    *ZERO
     C                   Z-ADD     EMEFD3        DLEFDT
     C     EMEFD2        WHENNE    *ZERO
     C                   Z-ADD     EMEFD2        DLEFDT
     C     EMEFD1        WHENNE    *ZERO
     C                   Z-ADD     EMEFD1        DLEFDT
     C                   ENDSL
     C*
     C                   Z-ADD     DLEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DLEFDT
      *
     C                   SELECT
     C     EMCAD5        WHENNE    *ZERO
     C                   Z-ADD     EMCAD5        DLCADT
     C     EMCAD4        WHENNE    *ZERO
     C                   Z-ADD     EMCAD4        DLCADT
     C     EMCAD3        WHENNE    *ZERO
     C                   Z-ADD     EMCAD3        DLCADT
     C     EMCAD2        WHENNE    *ZERO
     C                   Z-ADD     EMCAD2        DLCADT
     C     EMCAD1        WHENNE    *ZERO
     C                   Z-ADD     EMCAD1        DLCADT
     C                   ENDSL
     C*
     C                   Z-ADD     DLCADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DLCADT
      *
     C                   Z-ADD     EMFEIN        DLEFID
      ***** BEG DEL ******* 01/31/02 **********************************
     C*********            MOVELEMCONT    DLECNT
     C*********            MOVELEMATTN    DLEATT
     C*********            MOVELEMCNT3    DLECN3
      ***** END DEL ******* 01/31/02 **********************************
      ***** BEG ADD ******* 01/31/02 **********************************
      ***** BEG ADD ******* 11/18/02 **********************************
     C                   MOVEL(P)  *BLANKS       DLC1EM
     C                   MOVEL(P)  *BLANKS       DLC2EM
     C                   MOVEL(P)  *BLANKS       DLC3EM
      ***** END ADD ******* 11/18/02 **********************************
      *
      * Get Employer Contact Names / phone
      *
     C                   MOVE      '1'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         DLECNT
      ***** BEG ADD ******* 11/18/02 **********************************
     C  N60              MOVEL(P)  ECEMAL        DLC1EM
      ***** END ADD ******* 11/18/02 **********************************
      *
     C                   MOVE      '2'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         DLEATT
      ***** BEG ADD ******* 11/18/02 **********************************
     C  N60              MOVEL(P)  ECEMAL        DLC2EM
      ***** END ADD ******* 11/18/02 **********************************
      *
     C                   MOVE      '3'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         DLECN3
      ***** BEG ADD ******* 11/18/02 **********************************
     C  N60              MOVEL(P)  ECEMAL        DLC3EM
      ***** END ADD ******* 11/18/02 **********************************
      *
      ***** END ADD ******* 01/31/02 **********************************
     C                   MOVEL     EMFDNM        DLEDBA
     C                   MOVEL     EMCOM1        DLECM1
     C                   MOVEL     EMCOM2        DLECM2
     C                   MOVEL     EMCOM3        DLECM3
     C                   MOVEL     EMSEMP        DLESEM
     C                   MOVEL     EMRSK#        DLERSK
     C**
     C                   MOVEL     EMTPOB        DLEBUS
     C                   MOVEL     'BUSI'        XXTBID
     C                   MOVEL(P)  EMTPOB        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        DLEBUD
     C*
     C                   MOVEL     EMNAOB        DLENOB
     C                   MOVEL     EMIADR        DLIADR
     C**
     C                   MOVEL     EMSIC         DLSIC
     C                   MOVEL(P)  EMSIC         XXKEY
     C*** BEG CHG *** 08/15/00 ****************************
     C                   MOVEL(P)  'SIC'         XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        DLSICD
     C                   ENDIF
     C********** TBKY      CHAINPSC01                60
     C**N60*****           MOVELSCDESC    DLSICD    P
     C*** END CHG *** 08/15/00 ****************************
      *
     C                   Z-ADD     EMEFD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DLDT5
     C*
     C*  RETRIEVE THE CLAIMANT STAFF CODES
     C*
     C     CMKY          CHAIN     PCM03                              50
     C     *IN50         IFEQ      '0'
     C*
     C*  RETRIEVE THE CLAIMS SUPERVISOR NAME
     C*
     C     CMSUPR        CHAIN     PUP01                              50
     C  N50              MOVEL     UPUSNM        DLSUNM
     C*
     C*  RETRIEVE THE CLAIMS EXAMINER NAME
     C*
     C     CMCLEX        CHAIN     PUP01                              50
     C  N50              MOVEL     UPUSNM        DLCENM
     C*
     C*  RETRIEVE THE FIELD ADJUSTOR NAME
     C*
     C     CMFEAD        CHAIN     PUP01                              50
     C  N50              MOVEL     UPUSNM        DLFANM
     C*
     C*  RETRIEVE THE UNIT MANAGER NAME
     C*
     C*          CMUMAN    CHAINPUP01                50
     C* N50                MOVELUPUSNM    DLUMNM
     C*
     C                   END
      *
      ***** BEG ADD ******* 11/18/02 **********************************
     C                   MOVEL(P)  *BLANKS       DLAGEM
     C                   MOVEL(P)  *BLANKS       DLAPEM
      ***** END ADD ******* 11/18/02 **********************************
     C*
     C*   GET AGENT HISTORY INFORMATION:
     C     AHKEY         SETGT     PAH01                                        CO/FND/EMP/DIV/FYR
     C                   READP     PAH01                                  50
     C     *IN50         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  50
     C                   END                                                    END 0 *IN50
     C*
     C     *IN50         IFEQ      '0'
     C     AHCO#         ANDEQ     DLCO#
     C     AHFND         ANDEQ     DLFND
     C     AHEMP#        ANDEQ     DLEMP#
     C     AHDIV         ANDEQ     DLDIV
     C     AHFYR         ANDEQ     DLFYR
     C                   Z-ADD     AHAGT#        DLAAGT
     C*
     C*   CONVERT ELPTDT FROM YYMMDD TO MMDDYY
     C                   Z-ADD     AHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DLAEFD
     C*
     C                   Z-ADD     AHPROD        DLAPRO
     C*
     C*   GET AGENT ASSIGNMENT INFORMATION:
     C     AAKEY         CHAIN     PAA01                              50        CO/FND/AGT
     C     *IN50         IFEQ      '0'
     C                   Z-ADD     AAFEIN        DLAFID
     C                   Z-ADD     AAMOD         DLAMOD
     C                   END                                                    END 0 *IN50
     C*
     C*  GET AGENT PRODUCER NAME
     C*
     C     APKEY         CHAIN     PAP01                              50        CO/FEIN/MOD/PROD
     C     *IN50         IFEQ      '0'
      ***** BEG ADD ******* 11/18/02 **********************************
     C                   MOVEL(P)  APEMAL        DLAPEM
      ***** END ADD ******* 11/18/02 **********************************
     C                   MOVE      *BLANKS       QQLAST
     C                   MOVEL     APLSN         QQLAST           56
     C                   MOVEL     APFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        DLAPNM
     C                   END                                                    END 0 *IN50
     C*
     C*   GET AGENT MASTER INFORMATION:
     C     AGKEY         CHAIN     PAG01                              50        CO/FEIN/MOD
      *
     C     *IN50         IFEQ      '0'
      ***** BEG ADD ******* 11/18/02 **********************************
     C                   MOVEL(P)  AGEMAL        DLAGEM
      ***** END ADD ******* 11/18/02 **********************************
      **** Begin Change **** 09/16/14 *********************************
     C***                MOVEL     AGNAME        DLANME
      /Free
       DLANME = %subst(AGNAME:1:30);
      /End-free
      **** End   Change **** 09/16/14 *********************************
      ***** BEG ADD ******* 08/05/99 **********************************
     C                   Z-ADD     AGFAXA        DLAARF
     C                   Z-ADD     AGFAX#        DLAPHF
      ***** END ADD ******* 08/05/99 **********************************
     C*
     C*  AGENT MAILING ADDRESS
     C**** Begin Change **** 09/16/14 *********************************
     C****               MOVEL     AGMAD1        DLAMA1
      /Free
       DLAMA1 = %subst(AGMAD1:1:34);
      /End-Free
     C**** End Change ****   09/16/14 *********************************
     C*
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      AGMST         XXSTAT
     C                   Z-ADD     AGMZIP        XXZIP
     C                   MOVEL     AGMCTY        XXCTY
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         DLAMCS
     C*
     C*  AGENT PHYSICAL ADDRESS
     C                   MOVEL     AGPAD1        DLAPA1
     C*
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      AGPST         XXSTAT
     C                   Z-ADD     AGPZIP        XXZIP
     C                   MOVEL     AGPCTY        XXCTY
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         DLAPCS
     C*
     C*  AGENT ADDITIONAL INFORMATION
     C                   Z-ADD     AGARE1        DLAAR1
     C                   Z-ADD     AGPHN1        DLAPH1
     C                   Z-ADD     AGARE2        DLAAR2
     C                   Z-ADD     AGPHN2        DLAPH2
     C                   MOVE      AGSECY        DLASEC
     C                   MOVE      AGCONT        DLACNT
     C                   MOVE      AGCOMM        DLACM1
     C                   MOVE      AGCOM2        DLACM2
     C                   END                                                    END 0 *IN50
     C                   END                                                    END 0 *IN50
     C*
     C*   ESTABLISH SEQ#
     C     XXSEQ#        ADD       1             XXSEQ#
     C                   Z-ADD     XXSEQ#        DLSEQ#
      ***** BEG ADD ******* 11/18/02 **********************************
      *
      * User email
      *
     C     Q1USER        CHAIN     PUP02                              60
     C*** Begin Chg *** 10/03/14 ******************************************
     C**N60*****         MOVEL(P)  UPMAIL        DLUPEM
     C***60*****         MOVEL(P)  *BLANKS       DLUPEM
     C                   Clear                   DLupEm
     C                   Clear                   DLphnA
     C                   Clear                   DLphn#
     C                   If        *in60 = *Off
     C                   Eval      DLupEm = UPmail
     C                   Eval      DLphnA = UPphnA
     C                   Eval      DLphn# = UPphn#
     C                   EndIf
     C*** End   Add *** 10/03/14 ******************************************
      *
      * Employer email
      *
     C     EMKEY         CHAIN     PEM01                              60        CO/FND/EMP/DIV
     C  N60              MOVEL(P)  EMIADR        DLEMEM
     C   60              MOVEL(P)  *BLANKS       DLEMEM
      *
      ***** END ADD ******* 11/18/02 **********************************
     C*
     C*   WRITE DEPARTMENT LETTERS WORKFILE RECORD FOR PRINTING
     C                   WRITE     PDL02
     C*
     C*   UPDATE/WRITE POLICY LETTERS AUX FILE
     C*
     C     XXD2P         IFEQ      'Y'
     C                   UPDATE    PD201
     C                   ELSE
     C                   WRITE     PD201
     C                   END
     C*
     C**  WRITE RECORD TO DARTS FOR AGENT IF DEPARTMENT CODE = 'MRKTG' ***
     C*
     C     DLDPCD        IFEQ      'MRKTG'
     C*
     C**  ESTABLISH KEY
     C*** BEGIN DEL *** 04/26/00 **************************************
     C********             MOVE *BLANKS   CMKEY
     C*** END DEL *** 04/26/00 ****************************************
     C                   MOVE      *BLANKS       QAKEY
     C                   Z-ADD     AAFEIN        QAFEIN
     C                   Z-ADD     AAMOD         QAMOD
     C**  ESTABLISH RECORD
     C*** BEGIN DEL *** 04/26/00 **************************************
     C********             MOVELQAKEY     CMKEY
     C********             MOVE *BLANKS   CMCSID
     C********             MOVELXXDESC    CMCOMM
     C********             MOVE DLDCID    CMCOMM
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMCDAT
     C********             MOVELDORMTY    CMCODE
     C********             MOVELDORMCD    CMRMCD
     C********
     C***W0050R IS KEYED PGM USED FOR SMKDP KEY DEFINITION FILE.
     C********             MOVEL'W0040R'  CMPNAM           KEYED PGM
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMTRDT
     C********             Z-ADDDSTIME    CMTRTM
     C********             MOVE *ZEROS    CMSECL
     C********             MOVE 'N'       CMSBYL
     C********             MOVE *BLANKS   CMSTAT
     C********             MOVELDLIUSR    CMUSER
     C********             MOVEL'W4117R'  CMPGM
     C********             Z-ADDDSTIME    CMTIME
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMDT
     C********             MOVELDLIUSR    CMIUSR
     C********             MOVE *ZEROS    CMDCDT
     C********             MOVE *BLANKS   CMDIFL
     C********
     C********             MOVE 'WC'      CMAPID
     C********             MOVE *BLANKS   CMIDOC
     C********             MOVE *BLANKS   CMIQUE
     C********             MOVE *BLANKS   CMIMGF
     C********             MOVE *BLANKS   CMIPTH
     C********             MOVE *ZEROS    CMPIDX
     C********             MOVE *BLANKS   CMQPTY
     C********             MOVE *BLANKS   CMDCTP
     C********             MOVEL'W4117R'  CMIPGM
     C********                                                         *********
     C****BEGIN DEL *** 02/23/98 ***********************************************
     C**********           MOVE *BLANKS   CMSMCD
     C****END*  DEL *** 02/23/98 ***********************************************
     C********                                                         *********
     C****BEGIN ADD *** 02/23/98 ***********************************************
     C********                                                         *********
     C********   CMRMCD    CHAINPCT01                60
     C********                                                         *********
     C********   *IN60     IFEQ *OFF
     C********             MOVE CTSMCD    CMSMCD
     C********             ENDIF                           *IN60=OFF
     C********                                                         *********
     C****END*  ADD *** 02/23/98 ***********************************************
     C********
     C********             MOVE *BLANKS   CMFL01
     C********             MOVE *BLANKS   CMFL02
     C********             MOVE *BLANKS   CMFL03
     C********             MOVE *BLANKS   CMFL04
     C********             MOVE *BLANKS   CMFL05
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMIDT
     C********
     C****GET*LAST SEQ# USED
     C********
     C********   C3KY      SETGTPCM0I
     C********             READPPCM0I                    60
     C********   *IN60     IFEQ '0'
     C********   QCKEY     ANDEQPIKEY
     C********   PISEQ#    ADD  1         CMSEQ#
     C********             ELSE
     C********             Z-ADD0         CMSEQ#
     C********             END
     C********             WRITEPCM01
     C******BEGIN ADD **** 07/01/99 ***********************************
     C********             FEOD SDCML
     C******END   ADD **** 07/01/99 ***********************************
     C*** END DEL *** 04/26/00 ****************************************
     C*** BEGIN ADD *** 04/26/00 **************************************
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QAKEY         Q#KEY            40
     C                   MOVEL(P)  XXDESC        Q#COMM          256
     C                   MOVE      DLDCID        Q#COMM
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  'W0040R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  DLIUSR        Q#USER           10
     C                   MOVEL(P)  'W4117R'      Q#PGM            10
     C                   MOVEL(P)  *BLANKS       Q#DIFL            1
     C                   MOVEL(P)  'W4117R'      Q#IPGM           10
     C                   MOVEL(P)  *BLANKS       Q#FL01            1
     C                   MOVEL(P)  *BLANKS       Q#FL02            1
     C                   MOVEL(P)  DORMCD        Q#RMCD            5
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  'WC'          Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANKS       Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANKS       Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     DORMTY        Q#CODE            2
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
     C*** END ADD *** 04/26/00 ****************************************
     C*
     C                   END                                                    DLDPCD=MRKTG
     C*
     C                   ENDSR
******* ADD 5/22/06 ****************************************************
     ***=====================================================================
     C     WRDDOC        BEGSR
     ***=====================================================================
     C*** DETERMINE IF WE SHOULD DISPLAY OR EDIT FROM DO RECORD
     C*
     C                   MOVEL     *BLANK        DOALCH
     C     DOKEY         CHAIN     PDO01                              50
     C*
     C**C*****
     C                   MOVEA     DLDCID        CM3(15)                        DOCUMENT
     C*******              MOVEADLDCID    CM3,13           DOCUMENT
     C*** End   chg *** 03/05/01 **************************************
     C                   MOVE      DL#CPY        QQ#CPY            5
     C*** Begin chg *** 03/05/01 **************************************
     C                   MOVEA     QQ#CPY        CM3(37)                        COPIES
     C                   MOVEA     DLSDOC        CM3(67)                        DARTS DOC
     C*******              MOVEAQQ#CPY    CM3,35           COPIES
     C*******              MOVEADLSDOC    CM3,65           DARTS DOC
     C*** End   chg *** 03/05/01 **************************************
     C*
     C                   MOVEA     *BLANK        FLR
     C                   MOVEA     DLFLDR        FLR
     C                   Z-ADD     1             I                 3 0
     C*** Begin chg *** 03/05/01 **************************************
     C                   Z-ADD     86            II                3 0
     C*******              Z-ADD84        II      30
     C*** End   chg *** 03/05/01 **************************************
     C*
     C     FLR(I)        DOWNE     *BLANK
     C                   MOVEA     FLR(I)        CM3(II)
     C                   ADD       1             I
     C                   ADD       1             II
     C                   ENDDO
     C*
     C                   MOVEA     ''''          CM3(II)
     C                   ADD       1             II
     C*
     C                   MOVEA     ')'           CM3(II)
     C                   ADD       2             II
     C                   MOVEA     'SAVFLR('     CM3(II)
     C*
     C                   ADD       7             II
     C                   MOVEA     ''''          CM3(II)
     C*
     C                   MOVEA     *BLANK        FLR
     C                   MOVEA     DLSFLD        FLR
     C                   ADD       1             II
     C                   Z-ADD     1             I                 3 0
     C*
     C     FLR(I)        DOWNE     *BLANK
     C                   MOVEA     FLR(I)        CM3(II)
     C                   ADD       1             I
     C                   ADD       1             II
     C                   END
     C*
     C                   MOVEA     ''')'         CM3(II)
     C*
     C*   SPECIFY THE SELECTED OUTPUT QUEUE ON THE PRTDOC COMMAND
     C*
     C                   ADD       3             II
     C                   MOVEA     'OUTQ('       CM3(II)
     C                   ADD       5             II
     C                   MOVEA     DLOUTQ        CM3(II)
     C                   ADD       10            II
     C                   MOVEA     ')'           CM3(II)
     C*
     C                   ADD       2             II
     C                   MOVEA     'JOBQ'        CM3(II)
     C                   ADD       4             II
     C                   MOVEA     '(*NO)'       CM3(II)
     ****
     C     DOALCH        IFNE      'Y'
     C                   ADD       6             II
     C                   MOVEA     'EDTDSP'      CM3(II)
     C                   ADD       6             II
     C                   MOVEA     '(*DSP)'      CM3(II)
     C*
     C                   ELSE
     C*
     C                   ADD       6             II
     C                   MOVEA     '      '      CM3(II)
     C                   ADD       6             II
     C                   MOVEA     '      '      CM3(II)
     C*
     C                   ENDIF
    *****
     C                   MOVE      *BLANKS       CMDSTR
     C                   MOVEA     CM3           CMDSTR          225
     C                   Z-ADD     225           LENGTH           15 5
     C*
     C                   CALL      'QCMDEXC'                            37
     C                   PARM                    CMDSTR
     C                   PARM                    LENGTH
     C*
     C                   MOVE      *BLANKS       CMDSTR
     C*
     C*
     C     ENDWRD        ENDSR
******* END ADD 5/22/06 ****************************************************
     C* **************************************************
**  QCMDEXC COMMAND
SYPRTDOC DOC('            ') COPIES(     ) SAVOUTPUT(*YES) SAVDOC(
   ) FLR('

**  (CM2) QCMDEXC COMMAND
SYCHGDA   DOC('            ') ALWRPL(*NO) FLR('


**  (CM3) QCMDEXC COMMAND
DTWRDDOC DOC('            ') COPIES(     ) SAVOUTPUT(*YES) SAVDOC(
   ) FLR('

