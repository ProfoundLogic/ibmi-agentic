     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE F7098R - COPY PROVIDER BILL
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program copies an existing Provider Bill to a new  **
     H**   EOMB.                                                     **
     H**   CALLED FROM:                                              **
     H**     F0350R - Work With Provider Bills (Option 3)            **
     H**   CALLS TO:                                                 **
     H**     SRCVTDAT - Convert date from one format to another      **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED:   07/29/99      PROJECT: MC152                   **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H*  07/18/23 C:342 Project: NY060                                *
     H*                 Recompile - NY Compliance: CARC/RARC Update   *
     H*                                                               *
     H*  06/28/17 C:180 Support: 47295                                *
     H*
     H*  09/01/16 C:212 Project: FL091 Phase 2                        *
     H*                 Compile only                                  *
     H*                                                               *
     H*  07/26/16 C:079 Support: 42567/99                             *
     H*                 Compile only                                  *
     H*                                                               *
     H*  10/28/15 C:079 Support: 144/37740                            *
     H*                 Compile only                                  *
     H*                                                               *
     H*  07/07/11 C:251 Project: TX070                                *
     H*                 Compile only                                  *
     H*                                                               *
     H*  08/12/10 C:198 Project: 114MWS                               *
     H*                 Compile only                                  *
     H*                                                               *
     H*  11/14/08 C:212 Project: TX043                                *
     H*                 Compile only                                  *
     H*                                                               *
     H*  11/14/08 C:212 Project: TX043                                *
     H*                 Compile only                                  *
     H*                                                               *
     H*  06/21/01 C:62  Support: MC025 27930                          *
     H*                 CLEAR EANSNT, EACVI# AND EANPDT.              *
     H*                                                               *
     H*  01/10/00 C:56  Support: MC044 #20980                         *
     H*                 Load Syatem Date into FAEOP/EOTRDT            *
     H*                                                               *
     H*  10/29/99 C:79  Support: MC044/20347                          *
     H*                 1. Move an 'E' to EAEPFL so the bill won't be *
     H*                    picked up by the EOMB print program.       *
     H*                                                               *
     H*  10/28/99 C:79  Support: MC044/20346                          *
     H*            1. Use correct logical file to get next            *
     H*                    available CADSEQ number                    *
     H*                                                               *
     H*            2. If adjudication code is not override type,      *
     H*               clear PAID/MRA/OTH/OTH2/NTWK reductions fields. *
     H*               This will prevent extraneous information from   *
     H*               being written in these fields. If the user wants*
     H*               to change overrides they can in the detail pgm  *
     H*                                                               *
     H*            3. Use correct key list when reading FDDAP. It was *
     H*               reading by EOB# instead of DSEQ                 *
     H*                                                               *
     H*  10/18/99 C:31  Project: MC512                                *
     H*                 V4: Bill Entry re-write                       *
     H*                 1. Add EOCSDT, EOSTCD to fields copied        *
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      99      General usage - Record not found               **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      FAEAL1 (PEA01): EOMB Header Amount Transaction File    **
     H**                       (CO#/SUB#/FND/FYR/CASE/EOB#)          **
     H**      FAEDL  (PED01): Dental Bill Header info File           **
     H**                       (CO#/SUB#/FND/FYR/CASE/EOB#)          **
     H**      FAEEL  (PEE01): EOMB Header EOB/Notes Transaction File **
     H**                       (CO#/SUB#/FND/FYR/CASE/EOB#)          **
     H**      FAEHL  (PEH01): EOMB Phys/HCFA Header File             **
     H**                       (CO#/SUB#/FND/FYR/CASE/EOB#)          **
     H**      FAEOP  (PEO01): EOMB Transaction File                  **
     H**      FAEOLG (PEO02): EOMB Transaction File                  **
     H**                       (CO#/SUB#/FND/FYR/CASE/EOB#)          **
     H**      FAEPL  (PEP01): Pharmacy Bill Header info File         **
     H**                       (CO#/SUB#/FND/FYR/CASE/EOB#)          **
     H**      FAESL  (PES01): EOMB Status Change Transaction File    **
     H**                       (CO#/SUB#/FND/FYR/CASE/EOB#)          **
     H**      FAEUL  (PEU01): EOMB Hosp/UB Header File               **
     H**                       (CO#/SUB#/FND/FYR/CASE/EOB#)          **
     H**      FDCAL5 (PCA01): Detail Charge File-W/reasonable/custom **
     H**                       (CO#/SUB#/FND/FYR/CASE/EOB#/SEQ#)     **
     H**      FDDAL2 (PDA01): EOMB Detail Amount Transaction File    **
     H**                       (CO#/SUB#/FND/FYR/CASE/EOB#)          **
     H**      FDDDL  (PDD01): EOMB Dental Detail Transaction File    **
     H**                       (CO#/SUB#/FND/FYR/CASE/EOB#)          **
     H**      FDDEL  (PDE01): EOMB Detail EOB/Notes Transaction File **
     H**                       (CO#/SUB#/FND/FYR/CASE/EOB#)          **
     H**      FDDHL  (PDH01): EOMB Phys/HCFA Detail Transaction File **
     H**                       (CO#/SUB#/FND/FYR/CASE/EOB#)          **
     H**      FDDPL  (PDP01): EOMB Pharmacy Detail Transaction File  **
     H**                       (CO#/SUB#/FND/FYR/CASE/EOB#)          **
     H**      FDDUL  (PDU01): EOMB Hosp/UB Detail Transaction File   **
     H**                       (CO#/SUB#/FND/FYR/CASE/EOB#)          **
     H**      FMC1L  (PC101): Company Auxiliary File (CO#)           **
     H*****************************************************************
     H*
     FFAEOP     UF A E             DISK
     FFAEOLG    IF   E           K DISK
     F                                     RENAME(PEO01:PEO02)
     F                                     INFDS(DBFEOP)
     F*
     FFAEAL1    UF A E           K DISK
     F*
     FFAEDL     UF A E           K DISK
     F*
     FFAEEL     UF A E           K DISK
     F*
     FFAEHL     UF A E           K DISK
     F*
     FFAEPL     UF A E           K DISK
     F*
     FFAESL     UF A E           K DISK
     F                                     INFDS(DBFESP)
     F*
     FFAEUL     UF A E           K DISK
     F*
     FFDCAL5    UF A E           K DISK
     F*** BEG ADD *** 10/28/99 ****************************************
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0F)
     F*  Logical over all Tabel Files
     FFTABL     IF   E           K DISK
     F*** END ADD *** 10/28/99 ****************************************
     F*
     FFDDAL2    UF A E           K DISK
     F*
     FFDDDL     UF A E           K DISK
     F*
     FFDDEL     UF A E           K DISK
     F*
     FFDDHL     UF A E           K DISK
     F*
     FFDDPL     UF A E           K DISK
     F*
     FFDDUL     UF A E           K DISK
     F*
     FFMC1L     UF   E           K DISK
     F*
     F*** BEG ADD *** 06/28/17 ****************************************
     FFMBEL     IF   E           K DISK
     F*** END ADD *** 06/28/17 ****************************************

     D DBFEOP          DS
     D  DBREOP               397    400B 0
     D DBFESP          DS
     D  DBRESP               397    400B 0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*  FAESP Reason
     D                 DS
     D  XXREAS                 1     27
     D  XXREAT                 1     18    INZ('Copied from EOMB #')
     D  XXREAE                19     27
      *
      *** Beg Add *** 06/28/17 **************************************
     D GetClientID     pr                  extpgm('RTVCLTID')
     D                                3
      *
     D ClientId        S              3    INZ
      *** End Add *** 06/28/17 **************************************
     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0          Old FAEOP
     C                   PARM                    @@RRN2            9 0          New FAESP
     C                   PARM                    @@RRN3            9 0          New FAEOP
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     GENKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     GENKY1        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     GENKY2        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    CADSEQ
      *
      *  Claim Status Code Table File
     C     TCSKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    TABKEY           10
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CADSEQ        XXNSEQ
     C     *LIKE         DEFINE    CALIN#        XXLIN#
     C     *LIKE         DEFINE    EATRDT        XXTRDT
     C     *LIKE         DEFINE    EACHRG        XXCHRG
     C     *LIKE         DEFINE    EAMRA         XXMRA
     C     *LIKE         DEFINE    EAOTHR        XXOTHR
     C     *LIKE         DEFINE    EAOTH2        XXOTH2
     C     *LIKE         DEFINE    EANDIS        XXNDIS
     C     *LIKE         DEFINE    EAPAID        XXPAID
     C     *LIKE         DEFINE    EAFEIN        XXFEIN
     C     *LIKE         DEFINE    EAMOD         XXMOD
     C     *LIKE         DEFINE    EOSS#         KKSS#
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EODT          XXDT
     C     *LIKE         DEFINE    DESEQ#        XXSEQ#
     C*** BEG ADD *** 10/28/99 ****************************************
     C     *LIKE         DEFINE    CAMRA         TTMRA
     C     *LIKE         DEFINE    CAOTHR        TTOTHR
     C     *LIKE         DEFINE    CAOTH2        TTOTH2
     C     *LIKE         DEFINE    CANDIS        TTNDIS
     C     *LIKE         DEFINE    CAPAID        TTPAID
     C*** END ADD *** 10/28/99 ****************************************
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
      /Free
       // Begin Add *** 06/28/17 **************************************
       //          Get Client ID
       //          ------------------------------------------------------
          GetClientId(ClientId);
       // End    Add *** 06/28/17 **************************************
      /End-Free
     C*          Initialize key
     C*          ------------------------------------------------------
     C     @@RRN         CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C*
     C*          Get next EOB#
     C*          ------------------------------------------------------
     C     KKCO#         CHAIN     PC101                              99
     C     *IN99         IFEQ      *OFF
     C     C1EOB#        ADD       1             C1EOB#
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C                   MOVEL(P)  Q1USER        C1USER
     C                   MOVEL(P)  Q1PGM         C1PGM
     C                   MOVEL(P)  DSTIME        C1TIME
     C                   MOVEL(P)  XXDT          C1DT
     C                   UPDATE    PC101
     C*
     C*          ******************************************************
     C*          COPY FAEAP
     C*          ******************************************************
     C                   SETOFF                                       98
     C     GENKY         SETLL     PEA01
     C     GENKY         READE     PEA01                                  99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     0             XXCHRG
     C                   Z-ADD     0             XXMRA
     C                   Z-ADD     0             XXOTHR
     C                   Z-ADD     0             XXOTH2
     C                   Z-ADD     0             XXNDIS
     C                   Z-ADD     0             XXPAID
     C  N98              Z-ADD     EATRDT        XXTRDT
     C  N98              Z-ADD     EAFEIN        XXFEIN
     C  N98              Z-ADD     EAMOD         XXMOD
     C     *IN99         DOWEQ     *OFF
     C                   ADD       EACHRG        XXCHRG
     C                   ADD       EAMRA         XXMRA
     C                   ADD       EAOTHR        XXOTHR
     C                   ADD       EAOTH2        XXOTH2
     C                   ADD       EANDIS        XXNDIS
     C                   ADD       EAPAID        XXPAID
     C     GENKY         READE     PEA01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C                   Z-ADD     C1EOB#        EAEOB#
     C                   Z-ADD     0             EARCN
     C                   Z-ADD     XXTRDT        EATRDT
     C                   Z-ADD     XXCHRG        EACHRG
     C                   Z-ADD     XXMRA         EAMRA
     C                   Z-ADD     XXOTHR        EAOTHR
     C                   Z-ADD     XXOTH2        EAOTH2
     C                   Z-ADD     XXNDIS        EANDIS
     C                   Z-ADD     XXPAID        EAPAID
     C                   MOVE      'A'           EASTAT
     C                   MOVEL(P)  Q1USER        EAUSER
     C                   MOVEL(P)  Q1PGM         EAPGM
     C                   MOVEL(P)  DSTIME        EATIME
     C                   MOVEL(P)  XXDT          EADT
     C                   MOVEL     EAUSER        EAIUSR
     C                   MOVEL     EAPGM         EAIPGM
     C                   MOVEL     EATIME        EAITIM
     C                   MOVEL     EADT          EAIDT
     C                   Z-ADD     0             EAWCPC
     C                   Z-ADD     0             EAWCPD
     C                   Z-ADD     0             EAWCBC
     C                   Z-ADD     0             EAWCBD
     C                   Z-ADD     0             EAWCNC
     C                   Z-ADD     0             EAWCND
     C                   Z-ADD     0             EABAMT
     C                   Z-ADD     0             EANAMT
     C                   MOVE      *BLANKS       EARMDS
     C                   MOVE      *BLANKS       EARMK1
     C                   MOVE      *BLANKS       EARMK2
     C                   MOVE      *BLANKS       EARMK3
     C                   MOVE      *BLANKS       EACKST
     C                   MOVE      *BLANKS       EATRCD
     C                   MOVE      *BLANK        EAPBRC
     C                   MOVE      *BLANK        EABHRF
     C                   MOVE      *BLANK        EABBLF
     C                   Z-ADD     0             EABINV
     C                   Z-ADD     0             EABCAM
     C                   MOVE      *BLANK        EANHRF
     C                   MOVE      *BLANK        EANBLF
     C                   Z-ADD     0             EANINV
     C                   Z-ADD     0             EANCAM
     C                   Z-ADD     0             EAESDT
     C*** BEG CHG *** 10/29/99 ****************************************
     C******************** MOVE *BLANK    EAEPFL
     C                   MOVE      'E'           EAEPFL
     C*** END CHG *** 10/29/99 ****************************************
     C                   MOVE      *BLANK        EABPFL
     C                   Z-ADD     XXFEIN        EAFEIN
     C                   Z-ADD     XXMOD         EAMOD
     C                   Z-ADD     0             EABIDT
     C                   Z-ADD     0             EANIDT
     C                   MOVE      *BLANK        EAAPPF
     C                   MOVE      *BLANK        EABDIF
     C                   MOVE      *BLANK        EANDIF
     C                   MOVE      *BLANK        EABR2
     C                   MOVE      *BLANKS       EATMFL
     C                   MOVE      *BLANK        EACNTL
     C*** BEGIN ADD *** 06/21/01 **************************************
     C                   MOVEL     *BLANKS       EANSNT
     C                   MOVEL     *BLANKS       EACVI#
     C                   Z-ADD     *ZEROS        EANPDT
     C*** END   ADD *** 06/21/01 **************************************
      /Free
       // Begin Add *** 06/28/17 ***********************************
       EAepdt = *zeros;
       // End   Add *** 06/28/17 ***********************************
      /End-Free
     C                   WRITE     PEA01
     C                   ENDIF                                                  *IN99 = Off
     C*
     C*          ******************************************************
     C*          COPY FAEDP
     C*          ******************************************************
     C     GENKY         SETLL     PED01
     C     GENKY         READE     PED01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     C1EOB#        EDEOB#
     C                   MOVEL(P)  Q1USER        EDUSER
     C                   MOVEL(P)  Q1PGM         EDPGM
     C                   MOVEL(P)  DSTIME        EDTIME
     C                   MOVEL(P)  XXDT          EDDT
     C                   MOVEL     EDUSER        EDIUSR
     C                   MOVEL     EDPGM         EDIPGM
     C                   MOVEL     EDTIME        EDITIM
     C                   MOVEL     EDDT          EDIDT
     C                   WRITE     PED01
     C     GENKY         READE     PED01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*          ******************************************************
     C*          COPY FAEEP
     C*          ******************************************************
     C     GENKY         SETLL     PEE01
     C     GENKY         READE     PEE01                                  99
     C                   Z-ADD     0             XXSEQ#
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     C1EOB#        EEEOB#
     C                   ADD       1             XXSEQ#
     C                   Z-ADD     XXSEQ#        EESEQ#
     C                   MOVEL(P)  Q1USER        EEUSER
     C                   MOVEL(P)  Q1PGM         EEPGM
     C                   MOVEL(P)  DSTIME        EETIME
     C                   MOVEL(P)  XXDT          EEDT
     C                   MOVEL     EEUSER        EEIUSR
     C                   MOVEL     EEPGM         EEIPGM
     C                   MOVEL     EETIME        EEITIM
     C                   MOVEL     EEDT          EEIDT
     C                   WRITE     PEE01
     C     GENKY         READE     PEE01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*          ******************************************************
     C*          COPY FAEHP
     C*          ******************************************************
     C     GENKY         SETLL     PEH01
     C     GENKY         READE     PEH01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     C1EOB#        EHEOB#
     C                   MOVEL(P)  Q1USER        EHUSER
     C                   MOVEL(P)  Q1PGM         EHPGM
     C                   MOVEL(P)  DSTIME        EHTIME
     C                   MOVEL(P)  XXDT          EHDT
     C                   MOVEL     EHUSER        EHIUSR
     C                   MOVEL     EHPGM         EHIPGM
     C                   MOVEL     EHTIME        EHITIM
     C                   MOVEL     EHDT          EHIDT
     C                   WRITE     PEH01
     C     GENKY         READE     PEH01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*          ******************************************************
     C*          COPY FAEPP
     C*          ******************************************************
     C     GENKY         SETLL     PEP01
     C     GENKY         READE     PEP01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     C1EOB#        EPEOB#
     C                   MOVEL(P)  Q1USER        EPUSER
     C                   MOVEL(P)  Q1PGM         EPPGM
     C                   MOVEL(P)  DSTIME        EPTIME
     C                   MOVEL(P)  XXDT          EPDT
     C                   MOVEL     EPUSER        EPIUSR
     C                   MOVEL     EPPGM         EPIPGM
     C                   MOVEL     EPTIME        EPITIM
     C                   MOVEL     EPDT          EPIDT
     C                   WRITE     PEP01
     C     GENKY         READE     PEP01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*          ******************************************************
     C*          WRITE FAESP
     C*          ******************************************************
     C                   Z-ADD     EOCO#         ESCO#
     C                   Z-ADD     EOSUB#        ESSUB#
     C                   Z-ADD     EOFND         ESFND
     C                   Z-ADD     EOFYR         ESFYR
     C                   Z-ADD     EOCASE        ESCASE
     C                   MOVEL(P)  EOSS#         ESSS#
     C                   Z-ADD     EOACDT        ESACDT
     C                   Z-ADD     C1EOB#        ESEOB#
     C                   MOVEL(P)  'P'           ESBLST
     C                   MOVEL(P)  'CPY'         ESSTCD
     C                   MOVE(P)   KKEOB#        XXREAE
     C                   DO        8             IX                3 0
     C                   MOVEL     XXREAE        XXWK1             1
     C     XXWK1         IFNE      '0'
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  WK1 <> 0
     C                   MOVE      XXREAE        XXWK8             8
     C                   MOVEL(P)  XXWK8         XXREAE
     C                   ENDDO                                                  Do 8
     C                   MOVEL(P)  XXREAS        ESREAS
     C                   Z-ADD     0             ESRCN
     C                   Z-ADD     0             ESRRDT
     C                   Z-ADD     0             ESRSDT
     C                   MOVE      *BLANK        ESSTAT
     C                   MOVEL(P)  Q1USER        ESUSER
     C                   MOVEL(P)  Q1PGM         ESPGM
     C                   MOVEL(P)  DSTIME        ESTIME
     C                   MOVEL(P)  XXDT          ESDT
     C                   MOVEL     ESUSER        ESIUSR
     C                   MOVEL     ESPGM         ESIPGM
     C                   MOVEL     ESTIME        ESITIM
     C                   MOVEL     ESDT          ESIDT
     C                   Z-ADD     0             ESDIAL
     C                   Z-ADD     0             ESDICM
     C                   Z-ADD     0             ESDIDT
     C                   MOVE      *BLANKS       ESDKEY
     C                   Z-ADD     0             ESDSEQ
     C                   Z-ADD     0             ESLTR#
     C                   WRITE     PES01
     C*
     C*          ******************************************************
     C*          COPY FAEUP
     C*          ******************************************************
     C     GENKY         SETLL     PEU01
     C     GENKY         READE     PEU01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     C1EOB#        EUEOB#
     C                   MOVEL(P)  Q1USER        EUUSER
     C                   MOVEL(P)  Q1PGM         EUPGM
     C                   MOVEL(P)  DSTIME        EUTIME
     C                   MOVEL(P)  XXDT          EUDT
     C                   MOVEL     EUUSER        EUIUSR
     C                   MOVEL     EUPGM         EUIPGM
     C                   MOVEL     EUTIME        EUITIM
     C                   MOVEL     EUDT          EUIDT
     C                   WRITE     PEU01
     C     GENKY         READE     PEU01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*          Get last sequence number for case (XXNSEQ)
     C*          ------------------------------------------------------
     C*** BEG CHG *** 10/28/99 ****************************************
     C********** GENKY1    SETGTPCA01
     C********** GENKY1    REDPEPCA01               N    99
     C     GENKY1        SETGT     PCA0F
     C     GENKY1        READPE(N) PCA0F                                  99
     C*** END CHG *** 10/28/99 ****************************************
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     CADSEQ        XXNSEQ
     C                   ELSE                                                   *IN99 = Off
     C                   Z-ADD     0             XXNSEQ
     C                   ENDIF                                                  *IN99 = Off
     C*** BEG ADD *** 10/28/99 ****************************************
     C*
     C* Initialize total reductions and total paid fields
     C                   Z-ADD     *ZERO         TTMRA
     C                   Z-ADD     *ZERO         TTOTHR
     C                   Z-ADD     *ZERO         TTOTH2
     C                   Z-ADD     *ZERO         TTNDIS
     C                   Z-ADD     *ZERO         TTPAID
     C*** END ADD *** 10/28/99 ****************************************
     C*
     C*          Loop until all FDCAP records for EOMB are read
     C*          ------------------------------------------------------
     C     GENKY         SETLL     PCA01
     C     GENKY         READE     PCA01                                  99
     C                   Z-ADD     0             XXLIN#
     C     *IN99         DOWEQ     *OFF
     C                   ADD       1             XXNSEQ
     C*** BEG ADD *** 10/28/99 ****************************************
      *
      * Determine if this is an override type
     C                   MOVEL(P)  CACSCD        TABKEY
      *
     C     TCSKY         CHAIN     PCS01                              60
      *
      * Default to adjudication code 1 if you don't get a hit
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1  '         CACSCD
     C                   MOVE      ' '           CSOVRD
     C                   ENDIF
      *
     C*** END ADD *** 10/28/99 ****************************************
     C*
     C*                    *****************************************
     C*                    COPY FDDAP
     C*                    *****************************************
     C     GENKY2        CHAIN     PDA01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     0             XXCHRG
     C                   Z-ADD     0             XXMRA
     C                   Z-ADD     0             XXOTHR
     C                   Z-ADD     0             XXOTH2
     C                   Z-ADD     0             XXNDIS
     C                   Z-ADD     0             XXPAID
     C*
     C*                    Loop until all records read for this seq #
     C*                    --------------------------------------------
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     DATRDT        XXTRDT
     C                   ADD       DACHRG        XXCHRG
     C*
     C*** BEG ADD *** 10/28/99 ****************************************
     C     CSOVRD        IFEQ      'Y'
     C*** END ADD *** 10/28/99 ****************************************
     C*
     C                   ADD       DAMRA         XXMRA
     C                   ADD       DAOTHR        XXOTHR
     C                   ADD       DAOTH2        XXOTH2
     C                   ADD       DANDIS        XXNDIS
     C                   ADD       DAPAID        XXPAID
     C*
     C*** BEG ADD *** 10/28/99 ****************************************
     C                   ENDIF                                                  CSOVRD NE Y
     C*** END ADD *** 10/28/99 ****************************************
     C*
     C*** BEG CHG *** 10/28/99 ****************************************
     C********** GENKY     READEPDA01                    99
     C     GENKY2        READE     PDA01                                  99
     C*** END CHG *** 10/28/99 ****************************************
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*                    Write summary record
     C*                    --------------------------------------------
     C                   Z-ADD     C1EOB#        DAEOB#
     C                   Z-ADD     0             DARCN#
     C                   Z-ADD     XXTRDT        DATRDT
     C                   Z-ADD     XXNSEQ        DADSEQ
     C                   Z-ADD     XXCHRG        DACHRG
     C                   Z-ADD     XXMRA         DAMRA
     C                   Z-ADD     XXOTHR        DAOTHR
     C                   Z-ADD     XXOTH2        DAOTH2
     C                   Z-ADD     XXNDIS        DANDIS
     C                   Z-ADD     XXPAID        DAPAID
     C                   MOVEL(P)  Q1USER        DAUSER
     C                   MOVEL(P)  Q1PGM         DAPGM
     C                   MOVEL(P)  DSTIME        DATIME
     C                   MOVEL(P)  XXDT          DADT
     C                   MOVEL     DAUSER        DAIUSR
     C                   MOVEL     DAPGM         DAIPGM
     C                   MOVEL     DATIME        DAITIM
     C                   MOVEL     DADT          DAIDT
     C                   WRITE     PDA01
     C                   ENDIF                                                  *IN99 = OFF
     C*
     C*                    ********************************************
     C*                    COPY FDDDP
     C*                    ********************************************
     C     GENKY2        CHAIN     PDD01                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     C1EOB#        DDEOB#
     C                   Z-ADD     XXNSEQ        DDDSEQ
     C                   MOVEL(P)  Q1USER        DDUSER
     C                   MOVEL(P)  Q1PGM         DDPGM
     C                   MOVEL(P)  DSTIME        DDTIME
     C                   MOVEL(P)  XXDT          DDDT
     C                   MOVEL     DDUSER        DDIUSR
     C                   MOVEL     DDPGM         DDIPGM
     C                   MOVEL     DDTIME        DDITIM
     C                   MOVEL     DDDT          DDIDT
     C                   WRITE     PDD01
     C     GENKY2        READE     PDD01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*                    ********************************************
     C*                    COPY FDDEP
     C*                    ********************************************
     C     GENKY2        CHAIN     PDE01                              99
     C                   Z-ADD     0             XXSEQ#
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     C1EOB#        DEEOB#
     C                   Z-ADD     0             DERCN
     C                   ADD       1             XXSEQ#
     C                   Z-ADD     XXSEQ#        DESEQ#
     C                   Z-ADD     XXNSEQ        DEDSEQ
     C                   MOVEL(P)  Q1USER        DEUSER
     C                   MOVEL(P)  Q1PGM         DEPGM
     C                   MOVEL(P)  DSTIME        DETIME
     C                   MOVEL(P)  XXDT          DEDT
     C                   MOVEL     DEUSER        DEIUSR
     C                   MOVEL     DEPGM         DEIPGM
     C                   MOVEL     DETIME        DEITIM
     C                   MOVEL     DEDT          DEIDT
     C                   WRITE     PDE01
     C     GENKY2        READE     PDE01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*                    ********************************************
     C*                    COPY FDDHP
     C*                    ********************************************
     C     GENKY2        CHAIN     PDH01                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     C1EOB#        DHEOB#
     C                   Z-ADD     XXNSEQ        DHDSEQ
     C                   MOVEL(P)  Q1USER        DHUSER
     C                   MOVEL(P)  Q1PGM         DHPGM
     C                   MOVEL(P)  DSTIME        DHTIME
     C                   MOVEL(P)  XXDT          DHDT
     C                   MOVEL     DHUSER        DHIUSR
     C                   MOVEL     DHPGM         DHIPGM
     C                   MOVEL     DHTIME        DHITIM
     C                   MOVEL     DHDT          DHIDT
     C                   WRITE     PDH01
     C     GENKY2        READE     PDH01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*                    ********************************************
     C*                    COPY FDDPP
     C*                    ********************************************
     C     GENKY2        CHAIN     PDP01                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     C1EOB#        DPEOB#
     C                   Z-ADD     XXNSEQ        DPDSEQ
     C                   MOVEL(P)  Q1USER        DPUSER
     C                   MOVEL(P)  Q1PGM         DPPGM
     C                   MOVEL(P)  DSTIME        DPTIME
     C                   MOVEL(P)  XXDT          DPDT
     C                   MOVEL     DPUSER        DPIUSR
     C                   MOVEL     DPPGM         DPIPGM
     C                   MOVEL     DPTIME        DPITIM
     C                   MOVEL     DPDT          DPIDT
     C                   WRITE     PDP01
     C     GENKY2        READE     PDP01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*                    ********************************************
     C*                    COPY FDDUP
     C*                    ********************************************
     C     GENKY2        CHAIN(N)  PDU01                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     C1EOB#        DUEOB#
     C                   Z-ADD     XXNSEQ        DUDSEQ
     C                   MOVEL(P)  Q1USER        DUUSER
     C                   MOVEL(P)  Q1PGM         DUPGM
     C                   MOVEL(P)  DSTIME        DUTIME
     C                   MOVEL(P)  XXDT          DUDT
     C                   MOVEL     DUUSER        DUIUSR
     C                   MOVEL     DUPGM         DUIPGM
     C                   MOVEL     DUTIME        DUITIM
     C                   MOVEL     DUDT          DUIDT
     C                   WRITE     PDU01
     C     GENKY2        READE     PDU01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*          ******************************************************
     C*          COPY FDCAP
     C*          ******************************************************
     C                   Z-ADD     XXNSEQ        CADSEQ
     C                   MOVE      *BLANK        CADELF
     C                   MOVE      'P'           CAFLAG
     C                   Z-ADD     0             CACHK#
     C                   Z-ADD     C1EOB#        CAEOB#
     C                   MOVEL(P)  Q1USER        CAUSER
     C                   MOVEL(P)  Q1PGM         CAPGM
     C                   MOVEL(P)  DSTIME        CATIME
     C                   MOVEL(P)  XXDT          CADT
     C                   MOVEL     CAUSER        CAIUSR
     C                   MOVEL     CAPGM         CAIPGM
     C                   MOVEL     CATIME        CAITIM
     C                   MOVEL     CADT          CAIDT
     C                   Z-ADD     0             CALTR#
     C                   Z-ADD     0             CARCN
     C                   ADD       1             XXLIN#
     C                   Z-ADD     XXLIN#        CALIN#
     C*** BEG ADD *** 10/28/99 ****************************************
     C     CSOVRD        IFNE      'Y'
     C*
     C                   Z-ADD     *ZERO         CAMRA
     C                   Z-ADD     *ZERO         CAOTHR
     C                   Z-ADD     *ZERO         CAOTH2
     C                   Z-ADD     *ZERO         CANDIS
     C                   Z-ADD     *ZERO         CAPAID
     C*
     C                   ELSE
     C*
     C* Sum up totals to write to FAEOP/FAEAP later.
     C                   ADD       CAMRA         TTMRA
     C                   ADD       CAOTHR        TTOTHR
     C                   ADD       CAOTH2        TTOTH2
     C                   ADD       CANDIS        TTNDIS
     C                   ADD       CAPAID        TTPAID
     C*
     C                   ENDIF                                                  CSOVRD NE Y
     C*** END ADD *** 10/28/99 ****************************************
     C*
     C                   WRITE     PCA01                                        FDCAP
     C*
     C*          Read next FDCAP record
     C*          ------------------------------------------------------
     C     GENKY         READE     PCA01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*          ******************************************************
     C*          COPY FAEOP
     C*          ******************************************************
      /Free
       // Begin Add **** 06/28/17 *************************************
          Chain (EOast:EOsys:EObtyp:EOstyp) FMbel;
          If %found(FMbel);
             EOtape = BEedi;
          Endif;

          If ClientId = '114';
             EOCRNE = 'Y';
             EOCRDO = 'N';
          Endif;
       // End   Add **** 06/28/17 *************************************

      /End-Free
     C                   SETON                                        99
     C                   Z-ADD     0             EOCHK#
     C                   Z-ADD     C1EOB#        EOEOB#
     C                   MOVE      *BLANK        EOSTAT
     C                   MOVEL(P)  Q1USER        EOUSER
     C                   MOVEL(P)  Q1PGM         EOPGM
     C                   MOVEL(P)  DSTIME        EOTIME
     C                   MOVEL(P)  XXDT          EODT
     C                   MOVEL     EOUSER        EOIUSR
     C                   MOVEL     EODT          EOIDT
     C                   Z-ADD     0             EOWCCK
     C                   MOVE      *BLANK        EOCLSF
     C                   Z-ADD     0             EOINV#
     C                   MOVE      *BLANKS       EOCKCN
     C                   MOVE      *BLANKS       EOSB01
     C                   MOVE      *BLANKS       EOSB02
     C                   MOVE      *BLANKS       EOSB03
     C                   MOVE      *BLANKS       EOSB04
     C                   MOVE      *BLANKS       EOSB05
     C                   MOVE      *BLANKS       EOSB06
     C                   MOVE      *BLANKS       EOSB07
     C                   MOVE      *BLANKS       EOSB08
     C                   MOVE      *BLANKS       EOSB09
     C                   MOVE      *BLANKS       EOSB10
     C                   Z-ADD     0             EOBAMT
     C                   MOVEL     EOPGM         EOIPGM
     C                   MOVEL     EOTIME        EOITIM
     C                   MOVE      'P'           EOBLST
     C                   Z-ADD     0             EORCN
     C*** BEGIN DEL *** 10/18/99 **************************************
     C********             Z-ADD0         EOCSDT
     C********             Z-ADD0         EOCRDT
     C*** END DEL *** 10/18/99 ****************************************
     C                   MOVE      *BLANKS       EOSID#
     C                   MOVE      *BLANKS       EOSINV
     C                   MOVE      *BLANKS       EOSEFT
     C                   MOVE      *BLANKS       EOEID#
     C                   Z-ADD     0             EONAMT
     C                   MOVE      *BLANKS       EOCNTL
     C                   MOVE      *BLANKS       EOSBDT
     C*** BEG ADD *** 10/28/99 ****************************************
     C                   Z-ADD     TTMRA         EOMRA
     C                   Z-ADD     TTOTHR        EOOTHR
     C                   Z-ADD     TTOTH2        EOOTH2
     C                   Z-ADD     TTNDIS        EONDIS
     C                   Z-ADD     TTPAID        EOPAID
     C*** END ADD *** 10/28/99 ****************************************
     C*** BEG ADD *** 01/10/00 ****************************************
     C                   MOVEL     XXDT          EOTRDT
     C*** END ADD *** 01/10/00 ****************************************
     C                   WRITE     PEO01                                        FAEOP
     C                   ENDIF                                                  *IN99 = Off
     C*** BEG ADD *** 10/28/99 ****************************************
     C*          Update FAEAP with total reductions/payment
     C*          ------------------------------------------------------
     C                   Z-ADD     C1EOB#        KKEOB#
     C*
     C     GENKY         CHAIN     PEA01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     TTMRA         EAMRA
     C                   Z-ADD     TTOTHR        EAOTHR
     C                   Z-ADD     TTOTH2        EAOTH2
     C                   Z-ADD     TTNDIS        EANDIS
     C                   Z-ADD     TTPAID        EAPAID
     C*
     C                   UPDATE    PEA01
     C*
     C*** BEG DEL *** 05/04/00 ****************************************
     C******************** ELSE
     C******************** EXCPTREA01
     C*** END DEL *** 05/04/00 ****************************************
     C*
     C                   ENDIF                                                  *IN99=OFF
     C*** END ADD *** 10/28/99 ****************************************
     C*
     C*          Pass back new relative record numbers
     C*          ------------------------------------------------------
     C                   Z-ADD     C1EOB#        KKEOB#
     C     GENKY         CHAIN(N)  PES01                              99
     C  N99              Z-ADD     DBRESP        @@RRN2
     C     GENKY         CHAIN     PEO02                              99
     C  N99              Z-ADD     DBREOP        @@RRN3
     C*
     C*          End of program
     C*          ------------------------------------------------------
     C                   ENDIF                                                  *IN99 = Off
     C                   MOVE      *ON           *INLR
     O*** BEG DEL *** 05/04/00 ****************************************
     O*****PEA01   E                REA01
     O*** END DEL *** 05/04/00 ****************************************
