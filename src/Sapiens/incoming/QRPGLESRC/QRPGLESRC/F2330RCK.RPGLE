     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  CREATED:                                                     *
     H*  7/05/12 C:280 PROJECT: 114MER COPIED FROM F2330RTX           *
     H*****************************************************************
     H*  REVISIONS                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     F*
     FF2330FCK  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     FFTEBPCK   UF A E             DISK
     F                                     RENAME(PEB01:DBRCDF)
     F                                     INFDS(DBFDS)
     F*
     F* CK: EOMB Descriptions Table
     F*
     FFTABLCK   IF   E           K DISK
     F*
     F* States Claims Status Code Table File by CK: CO#/KEY:AC/CS/EB/PA/PM/
     F*
     FSTABL     IF   E           K DISK
     F*
     F* System-Wide State Code Table File
     F*
     FFTGTL     IF   E           K DISK
     F*
     F* Generic table file - Co #, ID, Key
     F*
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     F*
     F* Generic table file
     F*
     FFTGIL     IF   E           K DISK
     F*
     F* Generic table ID file
     F*
     D/COPY SCOPYMT01D
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('EOMB Code Mai-
     D                                     ntenance')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ST              2
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     PGMLS         PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    XXTBID
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM      ' '           ##KC              1
     C     *INKL         PARM      ' '           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      @@ST          ##ST              2
     C                   PARM      @@MODE        ##CALL            1
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     EBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST
      *
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C     GIKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXTBID
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EBCO#         ##CO#
     C     *LIKE         DEFINE    EBKEY         KKKEY
     C     *LIKE         DEFINE    STKEY         KKST
     C     *LIKE         DEFINE    GTTBID        XXTBID                         ADD 01/26/00
     C     *LIKE         DEFINE    GTKEY         XXKEY                          ADD 01/26/00
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     EBSTAT        IFEQ      'I'
     C                   MOVE      'A'           EBSTAT
     C                   ELSE
     C                   MOVE      'I'           EBSTAT
     C                   ENDIF                                                  EBSTAT
     C*
     C                   MOVEL     Q1USER        EBUSER
     C                   MOVEL     Q1PGM         EBPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EBDT
     C*
     C                   MOVEL     DSTIME        EBTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C*
     C     DSPLYR        BEGSR
     C                   EXSR      NEXTSC
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C* EOMB CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCKEY         IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C* CHECK TO BE SURE EOMB CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCKEY         KKKEY
     C*
     C   40
     CANN90EBKY          CHAIN     PEB01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* EOMB CODE DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90EBDT          IFGT      SAVEDT
     C     EBDT          OREQ      SAVEDT
     C     EBTIME        ANDGT     SAVETM
     C                   SETON                                        9078
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
      /Free
       //*** Begin Add *** 04/18/18 *****************************************

       // Validate EOB X-REF codes
       erXREF = *Off;
       erXRF2 = *Off;
       msgXREF = *Blanks;
       msgXRF2 = *Blanks;

       If *in90= *off and SCxref = *Blanks and SCxrf2 <> *blanks;
          msgXREF = 'Invalid value';
          erXREF = *On;
          *in90 = *on;
       Endif;

       If *in90= *off and SCxrf2 = *Blanks and SCxref <> *blanks;
          msgXRF2 = 'Invalid value';
          erXRF2 = *On;
          *in90 = *on;
       Endif;
       //*** End   Add *** 04/18/18 *****************************************
      /End-Free
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C*
     C     ERRCK3        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* MKSTHD - Creates the state header
     C*================================================================
     C*
     C     MKSTHD        BEGSR
     C*
     C* DISPLAY STATE CODE AND STATE NAME IN HEADER
     C*
     C                   MOVEL     @@ST          KKST
     C                   MOVEL     @@ST          SCST
     C*
     C     STKY          CHAIN     PST01                              99
     C                   MOVEL(P)  STNAME        SCSTNM
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     EBKEY         SCKEY
     C                   MOVEL     EBDESC        SCDESC
     C                   MOVEL     EBDSC2        SCDSC2
     C                   MOVEL     EBSTAT        SCSTAT
      /Free
       //*** Begin Add *** 04/18/18 *****************************************
       SCxref = EBxref;
       SCxrf2 = EBxrf2;
       //*** End   Add *** 04/18/18 *****************************************
      /End-Free
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCKEY
     C                   MOVEL     *BLANKS       SCDESC
     C                   MOVEL     'A'           SCSTAT
     C                   MOVE      *BLANKS       SCDSC2
      /Free
       //*** Begin Add *** 04/18/18 *****************************************
       erXREF = *off;
       erXRF2 = *off;
       msgXREF = *Blanks;
       msgXRF2 = *Blanks;
       SCxref = *blanks;
       SCxrf2 = *blanks;
       //*** End   Add *** 04/18/18 *****************************************
      /End-Free
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         EBCO#
     C                   MOVEL     SCKEY         EBKEY
     C                   MOVEL     SCDESC        EBDESC
     C                   MOVEL     SCDSC2        EBDSC2
     C                   MOVEL     SCSTAT        EBSTAT
      /Free
       //*** Begin Add *** 04/18/18 **************************************
         EBxref = SCxref;
         EBxrf2 = SCxrf2;
       //*** End   Add *** 04/18/18 **************************************
      /End-Free
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* NEXTSC - PROCESS NEXT SCREEN - A/R INFORMATION
     C*================================================================
     C*
     C     NEXTSC        BEGSR
     C*
     C     @@ST          IFEQ      'CR'
     C*
     C     @@MODE        IFEQ      'D'
     C                   MOVEL     @@MODE        ##MODE
     C                   ELSE
     C                   MOVEL     'C'           ##MODE
     C                   ENDIF
     C*
     C     @@MODE        IFEQ      'A'
     C                   Z-ADD     DBRRN         ##RRN
     C                   ELSE
     C                   Z-ADD     @@RRN         ##RRN
     C                   ENDIF
     C*
     C                   CALL      'F2330RCR'    ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C                   SETON                                        90
     C                   EXSR      GETDTM
     C*
     C     @@MODE        IFEQ      'A'
     C     DBRRN         DELETE    DBRCDF                             99
     C                   MOVEL     *ON           OUTFLG
     C                   ENDIF
     C                   ENDIF
     C*
     C     *INKC         IFEQ      *ON
     C     @@MODE        IFEQ      'A'
     C     DBRRN         DELETE    DBRCDF                             99
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
      *
     C                   ENDSR
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C* Executing the sub-routine to make the state header.
     C*
     C                   EXSR      MKSTHD
     C*
     C                   EXSR      SETTBL                                       ADD 01/26/00
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C*
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        EBUSER
     C                   MOVEL     Q1PGM         EBPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EBDT
     C*
     C                   MOVEL     DSTIME        EBTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   EXSR      NEXTSC
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        EBUSER
     C                   MOVEL     Q1PGM         EBPGM
     C                   MOVEL     Q1USER        EBIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EBDT
     C*
     C                   MOVEL     SRDTO         EBIDT
     C*
     C                   MOVEL     DSTIME        EBTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      FTEBPCK
     C*
     C                   EXSR      NEXTSC
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* SETTBL - SET THE TABLE ID'S FOR THE STATE SPECIFIC GEN TABLE
     C*================================================================
     C*
     C     SETTBL        BEGSR
     C*
     C* DETERMINE THE TBID'S FOR XREF
     C*
     C     @@ST          CAT(P)    'BG':0        XREF              4
     C                   MOVEL(P)  XREF          XXTBID
     C     GIKY          CHAIN     PGI01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'ZZBG'        XREF
     C                   ENDIF
      *
      /Free
       //*** Begin Add *** 04/18/18 *****************************************
       xxTBID = xREF;
       selXREF = 'GTTBID=''' + xxTbId + ''' AND GTSTAT<>''I''';
       disXREF = *IN09;
       //*** End   Add *** 04/18/18 *****************************************
      /End-Free
      *
     C* DETERMINE THE TBID'S FOR XRF2
     C*
     C     @@ST          CAT(P)    'RE':0        XRF2              4
     C                   MOVEL(P)  XRF2          XXTBID
     C     GIKY          CHAIN     PGI01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'ZZRE'        XRF2
     C                   ENDIF
      /Free
       //*** Begin Add *** 04/18/18 *****************************************
       xxTBID = xRF2;
       selXRF2 = 'GTTBID=''' + xxTbId + ''' AND GTSTAT<>''I''';
       disXRF2 = *IN09;
       //*** End   Add *** 04/18/18 *****************************************
      /End-Free
     C                   ENDSR
