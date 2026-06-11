     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H* Program: F0481RFL - Dental (DWC-11) Header Information        *
     H*                                                               *
     H*****************************************************************
     H* Created on 01/14/2003 by c:62 for Project FL027               *
     H*****************************************************************
     H* Revisions:                                                    *
     H*
     H*   08/13/15 C:079 Project FL092                                *
     H*                  FL ICD10 Medical EDI changes.DSPF changed too*
     H*                                                               *
     H*   05/27/15 C:306 SUPPORT 34971                                *
     H*                  Removed the F2 button from F0481FFL          *
     H*                  screen action panel. (Recompile only).       *
     H*                                                               *
     H* 10/04/08 C:180 SUPPORT 65159
     H*                  Fourth position of FL Phy lic can be blank
     H*
     H*                                                               *
     H* 09/26/07 C:180   SUPPORT: 59201                              *
     H*                   Validate SCFLIC based on following formats:
     H*
     H*                   AANNNNNNNNNNN
     H*                   (OR)
     H*                   AAANNNNNNNNNN
     H*                   (OR)
     H*                   AAAANNNNNNNNN
     H*
     H*                   Left justify and space fill to end of field
     H*                   Key alpha prefix and numeric digits of
     H*                   license number
     H*                   Must be valid value
     H*                   DO NOT zero pad numeric portion
     H*
     H*  09/10/07 C:180   SUPPORT: 58610                              *
     H*                   Allow editing of zip code.
     H*
     H*
     H*  04/23/07 C:188 PROJECT:FL061
     H*                 SET EDITST TO ADJ OR POLICY STATE BASED ON EOCPST
     H*
     H*  11/08/05 C:163 PROJECT: 48MWK
     H*                 FL EDI CORRECTION LICENSE #
     H*
     H*  03/30/05 C:079 PROJECT: 46234                                *
     H*                 ADD DATE CARRIER RECEIVED BILL                *
     H*                                                               *
     H*  01/20/05 C:079 PROJECT: FL046                                *
     H*                 1. EASE EDIT ON LIC NUM                       *
     H*                                                               *
     H*  04/15/04 C:079 PROJECT: FL046                                *
     H*                 1. USE GENERIC TABLE FOR SCTAPE               *
     H*                 2. USE NEW PLACE OF TREATMENT TABLE ID        *
     H*                                                               *
     H*  04/28/03 C:079 PROJECT: FL043                                *
     H*                 EXPAND LIC# FIELD TO 13A                      *
     H*                                                               *
     H*  03/12/03 C:079 SUPPORT: MC044 36319                          *
     H*                 CHECK THE STATUS OF DIAG CODES                *
     H*                                                               *
     H*****************************************************************
     FF0481FFL  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FFAEDP     UF A E             DISK
     F                                     RENAME(PED01:DBRCDF)
     FFAEOP     UF   E             DISK
     FFAEAL1    IF   E           K DISK
     F*
     FFMCDL     IF   E           K DISK
     FFDCALF    IF   E           K DISK
     FFMDIL     IF   E           K DISK
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     FFMPLLFL   IF   E           K DISK
     FSTSTL1    IF   E           K DISK
     F*** Beg Add *** 08/13/15 ***************************************
     Ffmicd10l1 if   e           k disk
     F*** End Add *** 08/13/15 ***************************************
      /COPY SCOPYMT01D
     D*
     D*** BEG DELETE 09/26/07 *****************************************
     D****¢¢¢             LT     26  26  1               ALPHABET
     D*** END DELETE 09/26/07 *****************************************
     D*** BEG ADD *** 09/26/07 ****************************************
     D @A              S              1    DIM(26) CTDATA PERRCD(26)
     D @N              S              1    DIM(10) CTDATA PERRCD(10)
     D LL              S              1    DIM(11)
     D*** END ADD *** 09/26/07 ****************************************
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Dental (DWC-11) H-
     D                                     eader Information')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D*
     D*   Data structure of FL License # to make sure first two
     D*     characters of license # are alpha
     D                 DS
     D*** BEG CHG *** 04/28/03 ****************************************
     D*************************************** 1   9 SCLIC#
     D  SCLIC#                 1     13
     D*** END CHG *** 04/28/03 ****************************************
     D  SCLIC1                 1      1
     D  SCLIC2                 2      2
     D  SCLIC3                 3      3
     D  SCLIC4                 4      4
     D*** BEG ADD *** 09/26/07 ****************************************
     D  SCLICA                 3     13
     D  SCLICB                 4     13
     D  SCLICC                 5     13
     D*** END ADD *** 09/26/07 ****************************************
     D*** BEG DEL *** 04/28/03 ****************************************
     D*************************************** 5   9 SCLIC5
     D*** END DEL *** 04/28/03 ****************************************
     D*** BEG ADD *** 04/28/03 ****************************************
     D*****¢¢¢ BEGIN DELETE ***** 09/26/07 ****************************
     D*****¢¢¢¢                               5  11 SCLIC5
     D*****¢¢¢¢                               4  10 SCLIC6
     D*****¢¢¢¢                               3   9 SCLIC7
     D*****¢¢¢ END DELETE ******* 09/26/07 ****************************
     D*** END ADD *** 04/28/03 ****************************************
     D******** BEGIN ADD **********  09/10/07 *************************
     D*   Data structure
     D                 DS
     D  EOFLIC                 1      9
     D  EOFLI5                 1      5

     D******** END ADD ************  09/10/07 *************************
     D*** Beg Add *** 08/13/15 ***************************************
     D RtvICD10Code    pr                  extpgm('F2554R')
     D incall                         1
     D inKC                           1
     D inKL                           1
     D outICD10                       7
     D inLOST                         1
     D*
     D incall          s              1
     D inKC            s              1
     D inKL            s              1
     D outICD10        s              7
     D inLOST          s              1

     D kkicd10         s                   like(iccode)
     D kkind           s                   like(icind)
     D hldia7          s              7    inz
     D*** End Add *** 08/13/15 ***************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FOR FAEDP
     C                   PARM                    @@RRN2            9 0          FOR FAEOP
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     DIKY          KLIST
     C                   KFLD                    XXDIA
     C*
     C     EAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C     PLKY          KLIST
     C*** BEG CHG *** 11/08/05 ****************************************
     C******************** KFLD           KKLIC#  9
     C                   KFLD                    KKLIC#           13
     C*** END CHG *** 11/08/05 ****************************************
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST             10
     C*
     C*** Beg Add *** 08/13/15 ***************************************
     C*** KEY LIST FOR DIAGNOSIS CODE MASTER FILE
     C     fmicd10l1key  klist
     C                   kfld                    kkicd10
     C                   kfld                    kkind
     C*** End Add *** 08/13/15 ***************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CACO#         KKCO#
     C     *LIKE         DEFINE    CACO#         QQCO#
     C     *LIKE         DEFINE    CASUB#        KKSUB#
     C     *LIKE         DEFINE    CAFND         KKFND
     C     *LIKE         DEFINE    CAFYR         KKFYR
     C     *LIKE         DEFINE    CACASE        KKCASE
     C     *LIKE         DEFINE    CAFDAT        KKFDAT
     C     *LIKE         DEFINE    CACPT4        KKCPT4
     C     *LIKE         DEFINE    CASS#         KKSS#
     C     *LIKE         DEFINE    CAEOB#        KKEOB#
     C     *LIKE         DEFINE    EDDIA1        XXDIA
     C     *LIKE         DEFINE    EDDIA1        XXDIA1
     C     *LIKE         DEFINE    DILOST        XXLOST
     C     *LIKE         DEFINE    DIICD9        XXICD9
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    CAFDAT        HLRCVD
     C     *LIKE         DEFINE    CAACDT        HLACDT
     C     *LIKE         DEFINE    CAACDT        HLSYDT
     C*** BEG ADD *** 04/23/07 ****************************************
     C     *LIKE         DEFINE    EOAST         EDITST
     C*** END ADD *** 04/23/07 ****************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*  Make sure 1st Diag Code is populated & make sure that it is
     C*    a valid code; if 2nd, 3rd, & 4th Diag Codes are filled,
     C*    make sure they are valid.
     C*
     C     SCDIA1        IFNE      *BLANKS
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 08/13/15 ************************************
     C                   MOVEL     SCDIA1        XXDIA
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C*** BEG ADD *** 03/12/03 ****************************************
     C     DISTAT        OREQ      'I'
     C*** END ADD *** 03/12/03 ****************************************
     C                   SETON                                        9071
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOS1
     C                   MOVEL     DIDSC1        SCDSC1
     C                   MOVEL     DIDS1         SCDS1
     C                   ENDIF
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   Else
     C/free
         kkICD10 = SCDIA1;
         Chain (kkICD10:'D') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in71 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
     C                   EndIf
     C*** END ADD ****** 08/13/15 ************************************
     C                   ENDIF
     C*
     C     SCDIA2        IFNE      *BLANKS
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 08/13/15 ************************************
     C                   MOVEL     SCDIA2        XXDIA
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C*** BEG ADD *** 03/12/03 ****************************************
     C     DISTAT        OREQ      'I'
     C*** END ADD *** 03/12/03 ****************************************
     C                   SETON                                        9072
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOS2
     C                   MOVEL     DIDSC1        SCDSC2
     C                   MOVEL     DIDS1         SCDS2
     C                   ENDIF
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   Else
     C/free
         kkICD10 = SCDIA2;
         Chain (kkICD10:'D') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in72 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
     C                   EndIf
     C*** END ADD ****** 08/13/15 ************************************
     C                   ENDIF
     C*
     C     SCDIA3        IFNE      *BLANKS
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 08/13/15 ************************************
     C                   MOVEL     SCDIA3        XXDIA
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C*** BEG ADD *** 03/12/03 ****************************************
     C     DISTAT        OREQ      'I'
     C*** END ADD *** 03/12/03 ****************************************
     C                   SETON                                        9073
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOS3
     C                   MOVEL     DIDSC1        SCDSC3
     C                   MOVEL     DIDS1         SCDS3
     C                   ENDIF
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   Else
     C/free
         kkICD10 = SCDIA3;
         Chain (kkICD10:'D') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in73 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
     C                   EndIf
     C*** END ADD ****** 08/13/15 ************************************
     C                   ENDIF
     C*
     C     SCDIA4        IFNE      *BLANKS
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 08/13/15 ************************************
     C                   MOVEL     SCDIA4        XXDIA
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C*** BEG ADD *** 03/12/03 ****************************************
     C     DISTAT        OREQ      'I'
     C*** END ADD *** 03/12/03 ****************************************
     C                   SETON                                        9074
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOS4
     C                   MOVEL     DIDSC1        SCDSC4
     C                   MOVEL     DIDS1         SCDS4
     C                   ENDIF
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   Else
     C/free
         kkICD10 = SCDIA4;
         Chain (kkICD10:'D') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in74 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
     C                   EndIf
     C*** END ADD ****** 08/13/15 ************************************
     C                   ENDIF
     C*
     C* Make sure place of treatment is valid
     C*
     C*** BEG ADD *** 04/15/04 ****************************************
     C     SCPOTX        IFEQ      *BLANKS
     C                   SETON                                        9075
     C                   ENDIF
     C*** END ADD *** 04/15/04 ****************************************
     C     SCPOTX        IFNE      *BLANKS
     C*** BEG CHG *** 04/15/04 ****************************************
     C******************** MOVEL'FLPX'    KKTBID    P
     C     SCTAPE        IFEQ      'Y'
     C                   MOVEL(P)  'FLPX'        KKTBID
     C                   ELSE
     C                   MOVEL(P)  'FLET'        KKTBID
     C                   ENDIF
     C*** END CHG *** 04/15/04 ****************************************
     C                   MOVEL(P)  SCPOTX        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9075
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                              ON
     C*
     C*  Make sure License# isn't blank
     C*
     C     SCFLIC        IFEQ      *BLANKS
     C                   SETON                                        9076
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C*  Check to see if the Provider DB installed
     C*
     C                   MOVEL     'FL'          KKST
     C     STKY          CHAIN     PST01                              99
     C     *IN99         IFEQ      *OFF
     C     STBS01        ANDEQ     'Y'
     C     SCTAPE        ANDEQ     'Y'
     C*
     C* edit against the Prov Lic# database
     C*
     C                   MOVEL     SCFLIC        KKLIC#
     C     PLKY          CHAIN     PPL01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9077
     C                   ENDIF                                                  *IN99
     C*
     C                   ELSE                                                   STBS01=Y
     C*
     C     SCFLIC        IFNE      *BLANKS
     C*** BEG CHG *** 04/28/03 ****************************************
     C******************** MOVELSCFLIC    SCLIC#
     C                   MOVEL(P)  SCFLIC        SCLIC#
     C*** END CHG *** 04/28/03 ****************************************
      *****¢¢¢ BEGIN DELETE ******** 09/26/07 *************************
     C*****¢¢¢¢            Z-ADD1         X       20
     C*****¢¢¢¢  SCLIC1    LOKUPLT,X                     99
     C*****¢¢¢¢  *IN99     IFEQ *OFF
     C*****¢¢¢¢            SETON                     9078
     C*****¢¢¢¢            GOTO CK1END
     C*****¢¢¢¢            ENDIF
     C*****¢¢¢¢
     C*****¢¢¢¢            Z-ADD1         X
     C*****¢¢¢¢  SCLIC2    LOKUPLT,X                     99
     C*****¢¢¢¢  *IN99     IFEQ *OFF
     C*****¢¢¢¢            SETON                     9078
     C*****¢¢¢¢            GOTO CK1END
     C*****¢¢¢¢            ENDIF
     C*****¢¢¢¢            ENDIF                           STBS01=Y
     C*
     C* 3rd position can be alpha or numeric
     C*****¢¢¢¢
     C*****¢¢¢¢            TESTN          SCLIC3     99
     C*****¢¢¢¢
     C*****¢¢¢¢  *IN99     IFEQ *OFF
     C*****¢¢¢¢  SCLIC3    IFLT 'A'
     C*****¢¢¢¢  SCLIC3    ORGT 'Z'
     C*****¢¢¢¢
     C*****¢¢¢¢            SETON                     9079
     C*****¢¢¢¢            GOTO CK1END
     C*****¢¢¢¢
     C*****¢¢¢¢            ENDIF                           SCLIC2 IFLT
     C*****¢¢¢¢            ENDIF                           *IN60 = 0
     C*****¢¢¢¢
     C* 4TH position can be alpha or numeric
     C*****¢¢¢¢
     C*****¢¢¢¢            TESTN          SCLIC4     99
     C*****¢¢¢¢
     C*****¢¢¢¢  *IN99     IFEQ '0'
     C*****¢¢¢¢  SCLIC4    IFLT 'A'
     C*****¢¢¢¢  SCLIC4    ORGT 'Z'
     C*****¢¢¢¢
     C*****¢¢¢¢            SETON                     9080
     C*****¢¢¢¢            GOTO CK1END
     C*****¢¢¢¢
     C*****¢¢¢¢            ENDIF                           SCLIC2 IFLT
     C*****¢¢¢             ENDIF                           *IN60 = 0
     C*
     C* The last 7 digits MUST BE numeric
     C*
     C*** BEG ADD *** 04/28/03 ****************************************
     C*****¢¢¢¢            SELEC
     C*****¢¢¢¢  SCLIC3    WHGE 'A'
     C*****¢¢¢¢  SCLIC3    ANDLE'Z'
     C*****¢¢¢¢  SCLIC4    ANDGE'A'
     C*****¢¢¢¢  SCLIC4    ANDLE'Z'
     C*** END ADD *** 04/28/03 ****************************************
     C*****¢¢¢¢            TESTN          SCLIC5     99
     C*** BEG ADD *** 04/28/03 ****************************************
     C*****¢¢¢¢  SCLIC3    WHGE 'A'
     C*****¢¢¢¢  SCLIC3    ANDLE'Z'
     *****¢¢¢¢¢  SCLIC4    ANDGE'0'
     *****¢¢¢¢¢  SCLIC4    ANDLE'9'
     *****¢¢¢¢             TESTN          SCLIC6     99
     *****¢¢¢¢
     C*** BEG DEL *** 01/20/05 ****************************************
     C*********
     C*********  SCLIC3    WHGE '0'
     C*********  SCLIC3    ANDLE'9'
     C*********  SCLIC4    ANDGE'0'
     C*********  SCLIC4    ANDLE'9'
     C*********            TESTN          SCLIC7     99
     C*** END DEL *** 01/20/05 ****************************************
     C*****¢¢¢¢
     C*****¢¢¢             ENDSL
     C*** END ADD *** 04/28/03 ****************************************
     C*****¢¢¢¢
     C*****¢¢¢¢  *IN99     IFEQ *OFF
     C*****¢¢¢¢            SETON                     9081
     C*****¢¢¢¢            GOTO CK1END
     C*****¢¢¢¢            ENDIF
     C*
     C********¢¢¢¢ BEGIN ADD ********* 09/26/07 **********************
     C                   MOVE      *ZEROS        LCNT              5 0
     C*
     C* FIRST TWO CHARACTERS OF LICENSE NUMBER MUST BE ALPHA
     C*
     C     SCLIC1        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9078
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ADD       1             LCNT
      *
     C     SCLIC2        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9078
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ADD       1             LCNT              5 0
      *
     C* 3rd position can be alpha or numeric
      *
     C     SCLIC3        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *ON
     C                   ADD       1             LCNT              5 0
     C                   ELSE
     C                   MOVE      *IN40         SV40              1
     C     SCLIC3        LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        9079
     C                   GOTO      CK1END
     C                   ENDIF
     C                   MOVE      SV40          *IN40
     C                   ENDIF
      *
     C*
     C* 4TH position can be alpha or numeric
     C*
     C***** BEGIN ADD *****  10/04/08 *********************************
     C     SCLIC4        IFGT      *BLANKS
     C***** END ADD *******  10/04/08 *********************************
     C*
     C     SCLIC4        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *ON
      *
     C     LCNT          IFEQ      3
     C                   ADD       1             LCNT              5 0
     C                   ELSE
     C                   SETON                                        9077
     C                   GOTO      CK1END
     C                   ENDIF
      *
     C                   ELSE
     C                   MOVE      *IN40         SV40              1
     C     SCLIC4        LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        9077
     C                   GOTO      CK1END
     C                   ENDIF
     C                   MOVE      SV40          *IN40
     C                   ENDIF
     C***** BEGIN ADD *****  10/04/08 *********************************
     C                   ENDIF
     C***** END ADD *******  10/04/08 *********************************
      *
     C                   CLEAR                   LL
      *
     C                   SELECT
     C     LCNT          WHENEQ    2
     C                   MOVEA     SCLICA        LL
     C     LCNT          WHENEQ    3
     C                   MOVEA     SCLICB        LL
     C     LCNT          WHENEQ    4
     C                   MOVEA     SCLICC        LL
     C                   ENDSL
      *
     C     13            SUB       LCNT          MAX               3 0
     C                   Z-ADD     MAX           IX                3 0
      * Find 1st non-blank
     C                   DO        MAX
     C     LL(IX)        IFNE      *BLANKS
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             IX
     C                   ENDDO
      * Check numeric part
      *
      *
     C     LL(1)         IFGT      *ZERO
      *
     C                   DO        IX            IY                3 0
     C     LL(IY)        LOOKUP    @N                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9081
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
      *
     C                   ELSE
     C                   SETON                                        9081
     C                   ENDIF
      *
     C     *IN90         IFEQ      *ON
     C                   GOTO      CK1END
     C                   ENDIF
     C****** END ADD ******* 08/29/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****
     C                   ENDIF
     C********¢¢¢¢ END ADD *********** 09/26/07 **********************
     C                   ENDIF                                                  LIC#<>*BLANK
     C*
     C* Make sure that Data Subm flag is either "Y" or "N" & not blank.
     C*
     C     SCTAPE        IFEQ      *BLANKS
     C                   SETON                                        9083
     C                   GOTO      CK1END
     C                   ELSE
     C*** BEG ADD *** 04/15/04 ****************************************
     C                   MOVEL(P)  'FLES'        KKTBID
     C                   MOVEL(P)  SCTAPE        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        8490
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 04/15/04 ****************************************
     C*** BEG DEL *** 04/15/04 ****************************************
     C**********
     C********** SCTAPE    IFEQ 'Y'
     C********** SCTAPE    OREQ 'N'
     C**********           SETOF                     76
     C**********           ELSE
     C**********           SETON                     7690
     C**********           GOTO CK1END
     C**********           ENDIF
     C**********           ENDIF
     C*** END DEL *** 04/15/04 ****************************************
     C*
     C     CK1END        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCDIA1'
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   IF        SCICDIND = 'Y'
     C
     C** DIAGNOSIS CODE #1 SEARCH
      /Free
          HLDIA7 = SCDIA1;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa1 = HLDIA7;
             *in90 = *on;
             *in31 = *on;
          Else;
             scDIa1 = outICD10;
             *in90 = *on;
             *in32 = *on;
          Endif;

          If scDia1 <> *blanks;
            kkICD10 = scDIa1;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLOS1 = 'D';
              SCDSC1 = ICDSC1;
              SCDS1  = ICFCODE;
            Endif;
          Endif;
      /End-Free
     C                   ELSE
     C*** END ADD ****** 08/13/15 ************************************
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM      'D'           XXLOST
     C*
     C                   SETON                                        90
     C     XXICD9        IFNE      *BLANKS
     C*** BEG CHG ****** 08/13/15 ************************************
     C****************** MOVE      XXICD9        SCDIA1
     C                   EVAL      SCDIA1 = XXICD9
     C*** END CHG ****** 08/13/15 ************************************
     C                   MOVEL     XXLOST        SCLOS1
     C                   MOVEL     SCDIA1        XXDIA
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     DIDSC1        SCDSC1
     C                   MOVEL     DIDS1         SCDS1
     C                   ENDIF
     C                   SETON                                        32
     C                   ELSE
     C                   SETON                                        31
     C                   ENDIF
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   ENDIF
     C*** END ADD ****** 08/13/15 ************************************
     C*
     C     HFLD          WHENEQ    'SCDIA2'
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   If        SCICDIND = 'Y'
     C*
     C** DIAGNOSIS CODE #2 SEARCH
     C*
      /Free
          HLDIA7 = SCDIA2;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa2 = HLDIA7;
             *in90 = *on;
             *in32 = *on;
          Else;
             scDIa2 = outICD10;
             *in90 = *on;
             *in33 = *on;
          Endif;

          If scDia2 <> *blanks;
            kkICD10 = scDIa2;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLOS2 = 'D';
              SCDSC2 = ICDSC1;
              SCDS2  = ICFCODE;
            Endif;
          Endif;

      /End-Free
     C                   ELSE
     C*** END ADD ****** 08/13/15 ************************************
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM      'D'           XXLOST
     C*
     C                   SETON                                        90
     C     XXICD9        IFNE      *BLANKS
     C*** BEG CHG ****** 08/13/15 ************************************
     C*****************  MOVE      XXICD9        SCDIA2
     C                   EVAL      SCDIA2 = XXICD9
     C*** END CHG ****** 08/13/15 ************************************
     C                   MOVEL     XXLOST        SCLOS2
     C                   MOVEL     SCDIA2        XXDIA
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     DIDSC1        SCDSC2
     C                   MOVEL     DIDS1         SCDS2
     C                   ENDIF
     C                   SETON                                        33
     C                   ELSE
     C                   SETON                                        32
     C                   ENDIF
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   ENDIF
     C*** END ADD ****** 08/13/15 ************************************
     C*
     C     HFLD          WHENEQ    'SCDIA3'
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   If        SCICDIND = 'Y'
     C*
     C** DIAGNOSIS CODE #3 SEARCH
     C*
      /Free
          HLDIA7 = SCDIA3;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa3 = HLDIA7;
             *in90 = *on;
             *in33 = *on;
          Else;
             scDIa3 = outICD10;
             *in90 = *on;
             *in34 = *on;
          Endif;

          If scDia3 <> *blanks;
            kkICD10 = scDIa3;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLOS3 = 'D';
              SCDSC3 = ICDSC1;
              SCDS3  = ICFCODE;
            Endif;
          Endif;

      /End-Free
     C                   ELSE
     C*** END ADD ****** 08/13/15 ************************************
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM      'D'           XXLOST
     C*
     C                   SETON                                        90
     C     XXICD9        IFNE      *BLANKS
     C*** BEG CHG ****** 08/13/15 ************************************
     C*****************  MOVE      XXICD9        SCDIA3
     C                   EVAL      SCDIA3 = XXICD9
     C*** END CHG ****** 08/13/15 ************************************
     C                   MOVEL     XXLOST        SCLOS3
     C                   MOVEL     SCDIA3        XXDIA
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     DIDSC1        SCDSC3
     C                   MOVEL     DIDS1         SCDS3
     C                   ENDIF
     C                   SETON                                        34
     C                   ELSE
     C                   SETON                                        33
     C                   ENDIF
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   ENDIF
     C*** END ADD ****** 08/13/15 ************************************
     C*
     C     HFLD          WHENEQ    'SCDIA4'
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   If        SCICDIND = 'Y'
     C*
     C** DIAGNOSIS CODE #4 SEARCH
     C*
      /Free
          HLDIA7 = SCDIA4;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa4 = HLDIA7;
             *in90 = *on;
             *in34 = *on;
          Else;
             scDIa4 = outICD10;
             *in90 = *on;
             *in35 = *on;
          Endif;

          If scDia4 <> *blanks;
            kkICD10 = scDIa4;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLOS4 = 'D';
              SCDSC4 = ICDSC1;
              SCDS4  = ICFCODE;
            Endif;
          Endif;
      /End-Free
     C                   ELSE
     C*** END ADD ****** 08/13/15 ************************************
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM      'D'           XXLOST
     C*
     C                   SETON                                        90
     C     XXICD9        IFNE      *BLANKS
     C*** BEG CHG ****** 08/13/15 ************************************
     C****************** MOVE      XXICD9        SCDIA4
     C                   EVAL      SCDIA4 = XXICD9
     C*** END CHG ****** 08/13/15 ************************************
     C                   MOVEL     XXLOST        SCLOS4
     C                   MOVEL     SCDIA4        XXDIA
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     DIDSC1        SCDSC4
     C                   MOVEL     DIDS1         SCDS4
     C                   ENDIF
     C                   SETON                                        35
     C                   ELSE
     C                   SETON                                        34
     C                   ENDIF
     C*** BEG ADD ****** 08/13/15 ************************************
B01  C                   ENDIF
     C*** END ADD ****** 08/13/15 ************************************
     C*
     C     HFLD          WHENEQ    'SCPOTX'
     C*
     C     SCTAPE        IFEQ      'Y'
     C                   MOVEL     'FLPX'        ##TBID
     C                   ELSE
     C                   MOVEL     'FLET'        ##TBID
     C                   ENDIF
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*** BEG CHG *** 04/15/04 ****************************************
     C******************** PARM 'FLPX'    ##TBID
     C                   PARM                    ##TBID
     C*** END CHG *** 04/15/04 ****************************************
     C*
     C     ##RRN         IFEQ      *ZEROS
     C                   SETON                                        9035
     C                   ELSE
     C     ##RRN         CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTKEY         SCPOTX
     C                   SETON                                        9036
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##KC = '1'
     C*
     C     HFLD          WHENEQ    'SCFLIC'
     C                   MOVEL     'FL'          KKST
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C     STBS01        ANDEQ     'Y'
     C*
     C                   CALL      'F2395R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C*** BEG CHG *** 11/08/05 ****************************************
     C******************** PARM           ##LIC#  9
     C                   PARM                    ##LIC#           13
     C*** END CHG *** 11/08/05 ****************************************
     C*
     C     ##KC          IFEQ      *ON
     C     ##KL          OREQ      *ON
     C     ##LIC#        OREQ      *BLANKS
     C                   SETON                                        9036
     C                   ELSE
     C*** BEG CHG *** 04/28/03 ****************************************
     C*******************  MOVEL##LIC#    SCFLIC
     C                   MOVEL(P)  ##LIC#        SCFLIC
     C*** END CHG *** 04/28/03 ****************************************
     C                   SETON                                        9037
     C                   ENDIF                                                  ##LIC#=*BLAN
     C*
     C                   ELSE                                                   *IN60=*OFF
     C                   SETON                                        9082
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C     HFLD          WHENEQ    'SCTAPE'
     C*** BEG DEL *** 04/15/04 ****************************************
     C*******************  SETON                     9085
     C*** END DEL *** 04/15/04 ****************************************
     C*** BEG ADD *** 04/15/04 ****************************************
    C*** BEG DEL *** 12/22/06 ****************************************
     C***************      SETON                     37
    C*** END DEL *** 12/22/06 ****************************************
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'FLES'        ##TBID
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCTAPE
     C                   MOVEL(P)  GTDESC        SCDSDS
    C*** BEG ADD *** 12/22/06 ****************************************
     C                   SETON                                        38
    C*** END ADD *** 12/22/06 ****************************************
     C                   ENDIF
    C*** BEG ADD *** 12/22/06 ****************************************
     C                   ELSE
     C                   SETON                                        37
    C*** END ADD *** 12/22/06 ****************************************
     C                   ENDIF
     C*** END ADD *** 04/15/04 ****************************************
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*** BEG ADD *** 03/30/05 ****************************************
     C                   CLEAR                   SCCRDT
     C*** END ADD *** 03/30/05 ****************************************
     C*
     C                   MOVEL     EDCO#         KKCO#
     C                   MOVEL     EDSUB#        KKSUB#
     C                   MOVEL     EDFND         KKFND
     C                   MOVEL     EDFYR         KKFYR
     C                   MOVEL     EDCASE        KKCASE
     C*
     C                   MOVEL(P)  EDSUB#        XXSUB#            5
     C                   MOVEL(P)  EDFND         XXFND             3
     C                   MOVEL(P)  EDFYR         XXFYR             3
     C                   MOVEL(P)  EDCASE        XXCASE            7
     C*
     C     CAKY          CHAIN     PCA01                              60
     C                   MOVEL     CASS#         KKSS#
     C*
     C                   Z-ADD     CAACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C     CDKY          CHAIN     PCD01                              60
     C*
     C                   MOVE      EOEOB#        SCEOMB
     C                   MOVE      EOCNTL        SCCNTL
     C***** BEGIN ADD ****** 09/10/07 *********************************
     C                   MOVEL     EOFLIC        SCFZIP
     C***** END ADD ******** 09/10/07 *********************************
     C*** BEG ADD *** 03/30/05 ****************************************
     C                   Z-ADD     EOCRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCRDT
     C*** END ADD *** 03/30/05 ****************************************
     C*
     C                   MOVE      EDEDI         SCTAPE
     C*** BEG ADD *** 04/15/04 ****************************************
     C                   MOVEL(P)  'FLES'        KKTBID
     C                   MOVEL(P)  SCTAPE        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCDSDS
     C                   ELSE
     C                   CLEAR                   SCDSDS
     C                   ENDIF
     C*** END ADD *** 04/15/04 ****************************************
     C*** BEG CHG *** 04/28/03 ****************************************
     C******************** MOVE EDLIC#    SCFLIC
     C                   MOVEL(P)  EDLIC#        SCFLIC
     C*** END CHG *** 04/28/03 ****************************************
     C*** BEG ADD *** 04/28/03 ****************************************
     C     EDFLIC        IFNE      *BLANKS
     C                   MOVEL(P)  EDFLIC        SCFLIC
     C                   ENDIF
     C*** END ADD *** 04/28/03 ****************************************
     C                   MOVE      EDPOTX        SCPOTX
     C*
     C     EDDIA1        IFNE      *BLANKS
     C                   MOVEL     EDDIA1        XXDIA
     C     DIKY          CHAIN     PDI01                              99
     C                   MOVEL     DIDSC1        SCDSC1
     C                   MOVEL     DILOST        SCLOS1
     C                   MOVEL     DIDS1         SCDS1
     C                   MOVEL     EDDIA1        SCDIA1
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC1
     C                   MOVE      *BLANKS       SCLOS1
     C                   MOVE      *BLANKS       SCDS1
     C                   MOVE      *BLANKS       SCDIA1
     C                   ENDIF
     C*
     C     EDDIA2        IFNE      *BLANKS
     C                   MOVEL     EDDIA2        XXDIA
     C     DIKY          CHAIN     PDI01                              99
     C                   MOVEL     DIDSC1        SCDSC2
     C                   MOVEL     DILOST        SCLOS2
     C                   MOVEL     DIDS1         SCDS2
     C                   MOVEL     EDDIA2        SCDIA2
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC2
     C                   MOVE      *BLANKS       SCLOS2
     C                   MOVE      *BLANKS       SCDS2
     C                   MOVE      *BLANKS       SCDIA2
     C                   ENDIF
     C*
     C     EDDIA3        IFNE      *BLANKS
     C                   MOVEL     EDDIA3        XXDIA
     C     DIKY          CHAIN     PDI01                              99
     C                   MOVEL     DIDSC1        SCDSC3
     C                   MOVEL     DILOST        SCLOS3
     C                   MOVEL     DIDS1         SCDS3
     C                   MOVEL     EDDIA3        SCDIA3
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC3
     C                   MOVE      *BLANKS       SCLOS3
     C                   MOVE      *BLANKS       SCDS3
     C                   MOVE      *BLANKS       SCDIA3
     C                   ENDIF
     C*
     C     EDDIA4        IFNE      *BLANKS
     C                   MOVEL     EDDIA4        XXDIA
     C     DIKY          CHAIN     PDI01                              99
     C                   MOVEL     DIDSC1        SCDSC4
     C                   MOVEL     DILOST        SCLOS4
     C                   MOVEL     DIDS1         SCDS4
     C                   MOVEL     EDDIA4        SCDIA4
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC4
     C                   MOVE      *BLANKS       SCLOS4
     C                   MOVE      *BLANKS       SCDS4
     C                   MOVE      *BLANKS       SCDIA4
     C                   ENDIF
     C*** BEG ADD ****** 08/13/15 ************************************
      /Free
         scICDind = EDicdInd;
         If SCICDIND = 'Y';

           SCDIA1 = EDICD1001;
           If SCDIA1 <> *blanks;
              kkICD10 = SCDIA1;

              Chain (kkICD10:'D') FMICD10L1;
              If %Found(FMICD10L1);
                SCDSC1 = ICDSC1;
                SCDS1 = ICFCODE;
                SCLOS1 = 'D';
              EndIf;
           EndIf;

           SCDIA2 = EDICD1002;
           If SCDIA2 <> *blanks;
              kkICD10 = SCDIA2;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCDSC2 = ICDSC1;
                   SCDS2 = ICFCODE;
                   SCLOS2 = 'D';
                EndIf;
           EndIf;

           SCDIA3 = EDICD1003;
           If SCDIA3 <> *blanks;
              kkICD10 = SCDIA3;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCDSC3 = ICDSC1;
                   SCDS3 = ICFCODE;
                   SCLOS3 = 'D';
                EndIf;
           EndIf;

           SCDIA4 = EDICD1004;
           If SCDIA4 <> *blanks;
              kkICD10 = SCDIA4;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCDSC4 = ICDSC1;
                   SCDS4 = ICFCODE;
                   SCLOS4 = 'D';
                EndIf;
           EndIf;

E01      EndIf;
      /End-Free
     C*** END ADD ****** 08/13/15 ************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C*** BEG CHG ****** 08/13/15 ************************************
     C****************   MOVE      SCDIA1        EDDIA1
     C****************   MOVE      SCDIA2        EDDIA2
     C****************   MOVE      SCDIA3        EDDIA3
     C****************   MOVE      SCDIA4        EDDIA4
     C                   Eval      EDdia1 = scDia1
     C                   Eval      EDdia2 = scDia2
     C                   Eval      EDdia3 = scDia3
     C                   Eval      EDdia4 = scDia4
     C*** END CHG ****** 08/13/15 ************************************
     C*** BEG ADD ****** 08/13/15 ************************************
      /Free
        EDicdInd = scICDind;
        If  SCICDIND = 'Y';

          EDICD1001 = SCDIA1;
          EDICD1002 = SCDIA2;
          EDICD1003 = SCDIA3;
          EDICD1004 = SCDIA4;

E01     EndIf;
      /End-Free
     C*** END ADD ****** 08/13/15 ************************************
     C*
     C*** BEG CHG *** 04/28/03 ****************************************
     C******************** MOVELSCFLIC    EDLIC#
     C                   MOVEL     SCFLIC        EDFLIC
     C*** END CHG *** 04/28/03 ****************************************
     C                   MOVEL     SCTAPE        EDEDI
     C                   MOVEL     SCPOTX        EDPOTX
     C     EAKY          CHAIN     PEA01                              99
     C                   MOVEL     SCTAPE        EAEDI
     C*
     C                   MOVE      Q1USER        EDUSER
     C                   MOVE      Q1PGM         EDPGM
     C                   MOVE      DSTIME        EDTIME
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EDDT
     C*
     C*** BEG ADD *** 03/30/05 ****************************************
     C                   MOVE      SCCRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOCRDT
     C*** END ADD *** 03/30/05 ****************************************
     C***** BEGIN ADD ****** 09/10/07 *********************************
     C                   MOVE      SCFZIP        EOFLI5
     C***** END ADD ******** 09/10/07 *********************************
     C                   MOVE      Q1USER        EOUSER
     C                   MOVE      Q1PGM         EOPGM
     C                   MOVE      DSTIME        EOTIME
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C*
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   MOVE      DSTIME        EATIME
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C* SET OFF CURSOR POSITIONING INDICATORS
     C*
     C                   MOVEA     '00000000'    *IN(31)
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C** Get standard screen headings
     C*
     C     @@RRN2        CHAIN(N)  PEO01                              99
     C*
     C*** BEG ADD *** 04/23/07 ****************************************
     C*
     C*  SET EDITST TO POLICY OR ADJ STATE BASED ON CPST FLAG
     C*
     C     EOCPST        IFEQ      'C'
     C                   MOVEL(P)  EOPOST        EDITST
     C                   ELSE
     C                   MOVEL(P)  EOAST         EDITST
     C                   ENDIF
     C*
     C*** END ADD *** 04/23/07 ****************************************
     C*
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C                   MOVE      EOCO#         KKCO#
     C                   MOVE      EOSUB#        KKSUB#
     C                   MOVE      EOFND         KKFND
     C                   MOVE      EOFYR         KKFYR
     C                   MOVE      EOCASE        KKCASE
     C                   MOVE      EOEOB#        KKEOB#
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C*
     C                   CALL      'FA702R'
     C                   PARM                    ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    #1L1             78
     C                   PARM                    #1L2             78
     C*
     C                   MOVEL     #1L1          SCHD1
     C                   MOVEL     #1L2          SCHD2
     C*
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C                   CALL      'FA707R'
     C                   PARM                    ##RTCD
     C                   PARM                    @@RRN2
     C                   PARM                    #1L1
     C                   MOVEL     #1L1          SCHD3
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN2        CHAIN     PEO01                              99
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
     C                   UPDATE    PEO01
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FAEDP
     C*
     C                   ENDSR
**  TABLE @A  - ALPHABET
ABCDEFGHIJKLMNOPQRSTUVWXYZ
**
0123456789
