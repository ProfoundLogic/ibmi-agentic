     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*====================================================================
     H/TITLE API to Maintain Document Object Transaction File
     H*====================================================================
     H*  Created: 10/17/11   C:279  Project: SY195
     H*====================================================================
     H*  Revisions:
     H*
     H* 08/07/13 C:191 Redmine: 3548                                      *
     H*                  - Reset UNCN path for next call                  *
     H*
     H* 11/16/12 C:282 Project: SY195                                     *
     H*                  - Update the path for data center client.        *
     H*
     H*====================================================================
     F* Document Transaction File
     FIMDOP     O    E             DISK
     F*
     F* Document Index Transaction File
     FIMDOP1    O    E             DISK
     F*
     D*
      *** Beg Add ***** 11/16/12 **************************************
     D xxflrp          s             81
      *** End Add ***** 11/16/12 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D DSDTTM                        12  0
     D  DSTIME                        6  0 OVERLAY(DSDTTM)
     D  DSDATE                        6  0 OVERLAY(DSDTTM:7)
      *
      * Entry parameter list
     C     *ENTRY        PLIST
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFLR             8
     C                   PARM                    QQSFLR           12
     C                   PARM                    QQOSYS            1
     C                   PARM                    QQLIBN           10
     C                   PARM                    QQOFLD            8
     C                   PARM                    QQOSFL           12
     C                   PARM                    QQODCI           12
     C                   PARM                    QQARQS            4 0
     C                   PARM                    QQDTYP            8
     C                   PARM                    QQBYTD            5 1
     C                   PARM                    QQWSID            2
     C                   PARM                    QQSDT             8 0
     C                   PARM                    QQSTMD            6 0
     C                   PARM                    QQSREP            8
     C                   PARM                    QQBCHI           12
     C                   PARM                    QQPGS             4 0
     C                   PARM                    QQAIFL            1
     C                   PARM                    QQOPTF            1
     C                   PARM                    QQIDFX            1
     C                   PARM                    QQVOL1           10
     C                   PARM                    QQVOL2           10
     C                   PARM                    QQVOL3           10
     C                   PARM                    QQDKEY           40
     C                   PARM                    QQDSQ#            5 0
     C                   PARM                    QQFL01            1
     C                   PARM                    QQFL02            1
     C                   PARM                    QQFL03            1
     C                   PARM                    QQFL04            1
     C                   PARM                    QQFL05            1
     C                   PARM                    QQCHR5            5
     C                   PARM                    QQDSTS            2
     C                   PARM                    QQRCVD            8 0
     C                   PARM                    QQUNCN          255
     C                   PARM                    QQINDX            1
     C                   PARM                    QQRTRN            1
     C
     C* Parameter list for geeting Environment
     C     RTVENV        PLIST
     C                   PARM                    ENV               1
     C
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C* Parameter list for getting ROOT
     C     RTVROOT       PLIST
     C                   PARM                    ROOT             10
     C
     C*---------------------------------------------------------------
     C* Main Program
     C*---------------------------------------------------------------
      *** Beg Add ***** 11/16/12 **************************************
     c                   Call      'SA030R'
     c                   parm      *Blanks       qqsufx            3
     c                   parm      *Blanks       qqflrp           80
     c
     c                   eval      xxflrp = *Blanks
     c                   If        qqflrp <> *Blanks
     c                   eval      xxflrp = '/' + %trim(qqflrp)
     c                   EndIf
      *** End Add ***** 11/16/12 **************************************
     C
     C* Get IFS Path and environment
     C                   EXSR      GETIFS
     C
     C* Populate data base fields.
     C                   EXSR      MVTODB
     C
     C                   EVAL      *INLR = *ON
     C*-------------------------------------------------------------
     C*  GETIFS - Get IFS Path and environment
     C*-------------------------------------------------------------
     C     GETIFS        BEGSR
     C
     C                   CALL      'RTVDOCENV'   RTVENV
     C*
     C*************      IF        ENV = 'I' AND QQUNCN = *BLANKS AND
     C                   IF        QQUNCN = *BLANKS AND
     C                             (QQFLR <> ' ' OR QQSFLR <> ' ' OR
     C                              QQDCID <> ' ')
     C                   CALL      'RTVDOCROOT'  RTVROOT
     C                   EVAL      QQUNCN = '/' + %TRIM(ROOT) +
      *** Beg Add ***** 11/16/12 **************************************
     C                                            %Trim(XXFLRP) +
      *** End Add ***** 11/16/12 **************************************
     C                                      '/' + %TRIM(QQFLR)
     C                   IF        QQSFLR <> *BLANKS
     C                   EVAL      QQUNCN = %TRIM(QQUNCN) +
     C                                      '/' + %TRIM(QQSFLR)
     C                   ENDIF
     C                   EVAL      QQUNCN = %TRIM(QQUNCN) +
     C                                      '/' + %TRIM(QQDCID)
     C*
     C                   ENDIF
     C
     C                   ENDSR
     C*-------------------------------------------------------------
     C*  MVTODB - Populate data base fields.
     C*-------------------------------------------------------------
     C     MVTODB        BEGSR
     C
     C                   EVAL      DODCID  = QQDCID
     C                   EVAL      DOFLR   = QQFLR
     C                   EVAL      DOSFLR  = QQSFLR
     C                   EVAL      DOOSYS  = QQOSYS
     C                   EVAL      DOLIBN  = QQLIBN
     C                   EVAL      DOOFLD  = QQOFLD
     C                   EVAL      DOOSFL  = QQOSFL
     C                   EVAL      DOODCI  = QQODCI
     C                   EVAL      DOARQS  = QQARQS
     C                   EVAL      DODTYP  = QQDTYP
     C                   EVAL      DOBYTD  = QQBYTD
     C                   EVAL      DOWSID  = QQWSID
     C                   EVAL      DOSDT   = QQSDT
     C                   EVAL      DOSTMD  = QQSTMD
     C                   EVAL      DOSREP  = QQSREP
     C                   EVAL      DOBCHI  = QQBCHI
     C                   EVAL      DOPGS   = QQPGS
     C                   EVAL      DOAIFL  = QQAIFL
     C                   EVAL      DOOPTF  = QQOPTF
     C                   EVAL      DOIDFX  = QQIDFX
     C                   EVAL      DOVOL1  = QQVOL1
     C                   EVAL      DOVOL2  = QQVOL2
     C                   EVAL      DOVOL3  = QQVOL3
     C                   EVAL      DODKEY  = QQDKEY
     C                   EVAL      DODSQ#  = QQDSQ#
     C                   EVAL      DOFL01  = QQFL01
     C                   EVAL      DOFL02  = QQFL02
     C                   EVAL      DOFL03  = QQFL03
     C                   EVAL      DOFL04  = QQFL04
     C                   EVAL      DOFL05  = QQFL05
     C                   EVAL      DOCHR5  = QQCHR5
     C                   EVAL      DODSTS  = QQDSTS
     C                   EVAL      DORCVD  = QQRCVD
     C                   EVAL      DOUNCN  = QQUNCN
     C                   IF        QQUNCN <> ' '
     C                   EVAL      DOIFSP  = ENV
     C                   ENDIF
     C                   EVAL      DOIUSR  = Q1USER
     C                   EVAL      DOIPGM  = Q1PGM
     C                   TIME                    DSDTTM
     C                   EVAL      DOITIM  = DSTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DOIDT
     C*****************  EVAL      DOIDT   = DSDATE
     C                   CLEAR                   DOLUSR
     C                   CLEAR                   DOLPGM
     C                   CLEAR                   DOLTIM
     C                   CLEAR                   DOLDT
     C
     C                   WRITE     PDO01                                99
     C
     C                   IF        QQDKEY <> *BLANKS  AND QQINDX = 'Y'
     C                   WRITE     PDO011                               99
     C                   ENDIF
     C*** BEG Add *******08/07/13 *************************************
     C                   EVAL      QQUNCN = *BLANKS
     C*** END Add *******08/07/13 *************************************
     C*
     C* Validate if record is successfully added.
     C                   IF        *IN99
     C                   EVAL      QQRTRN = 'X'
     C                   ELSE
     C                   EVAL      QQRTRN = ' '
     C                   ENDIF
     C
     C                   ENDSR
