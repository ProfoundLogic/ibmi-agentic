     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H**********************************************************************
     H* Revisions:
     H*
     H* 07/29/20 C:180 Project: 152WBE  Add security types and levels to
     H*                         allow claims closing with outstanding PPO
     H*                         check requests.
     H**********************************************************************
     FW2142F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWTPSP     UF A E             DISK
     F                                     RENAME(PPS01:DBRCDF)
     FWTPSL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Payment Status Code-
     D                                      Master Information')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     PSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##KEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    PSCO#         ##CO#
     C     *LIKE         DEFINE    PSKEY         ##KEY
     C     *LIKE         DEFINE    PSKEY         @@KEY
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
     C     PSSTAT        IFEQ      'I'
     C                   MOVE      'A'           PSSTAT
     C                   ELSE
     C                   MOVE      'I'           PSSTAT
     C                   ENDIF                                                  PSSTAT
     C*
     C                   MOVEL     Q1USER        PSUSER
     C                   MOVEL     Q1PGM         PSPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PSDT
     C*
     C                   MOVEL     DSTIME        PSTIME
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
     C* PAYMENT STATUS CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCKEY         IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C* CHECK TO BE SURE PAYMENT TYPE CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCKEY         ##KEY
     C*
     C   40
     CANN90PSKY          CHAIN     PPS01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* PAYMENT TYPE CODE DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
     C*
     C* PAYMENT SORT SEQENCE CANNOT BE BLANK
     C*
     C  N90SCSRT         IFEQ      *BLANKS
     C                   SETON                                        74  90
     C                   ENDIF                                                  SCSRT
     C*
     C* PAYMENT STATUS TYPE MUST BE VALID.
     C*
     C  N90SCTYPE        IFNE      'P'
     C     SCTYPE        ANDNE     'R'
     C     SCTYPE        ANDNE     'D'
     C                   SETON                                        75  90
     C                   ENDIF                                                  SCTYPE
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90PSDT          IFGT      SAVEDT
     C     PSDT          OREQ      SAVEDT
     C     PSTIME        ANDGT     SAVETM
     C                   SETON                                        9078
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      /Free
       //*** Begin Add 07/29/20 *******************************************
        erTyp01 = *Off;
        erTyp02 = *Off;
        erTyp03 = *Off;
        erTyp04 = *Off;
        erTyp05 = *Off;
        erTyp06 = *Off;
        erTyp07 = *Off;
        erTyp08 = *Off;
        erTyp09 = *Off;
        erTyp10 = *Off;
        erLvl01 = *Off;
        erLvl02 = *Off;
        erLvl03 = *Off;
        erLvl04 = *Off;
        erLvl05 = *Off;
        erLvl06 = *Off;
        erLvl07 = *Off;
        erLvl08 = *Off;
        erLvl09 = *Off;
        erLvl10 = *Off;

        If scTyp01 = *blanks and scLvl01 > *Zeros;
           erTyp01 = *on;
           *in90 = *On;
        Endif;

        If scTyp02 = *blanks and scLvl02 > *Zeros;
           erTyp02 = *on;
           *in90 = *On;
        Endif;

        If scTyp03 = *blanks and scLvl03 > *Zeros;
           erTyp03 = *on;
           *in90 = *On;
        Endif;

        If scTyp04 = *blanks and scLvl04 > *Zeros;
           erTyp04 = *on;
           *in90 = *On;
        Endif;

        If scTyp05 = *blanks and scLvl05 > *Zeros;
           erTyp05 = *on;
           *in90 = *On;
        Endif;

        If scTyp06   = *blanks and scLvl06 > *Zeros;
           erTyp06 = *on;
           *in90 = *On;
        Endif;

        If scTyp07 = *blanks and scLvl07 > *Zeros;
           erTyp07 = *on;
           *in90 = *On;
        Endif;

        If scTyp08 = *blanks and scLvl08 > *Zeros;
           erTyp08 = *on;
           *in90 = *On;
        Endif;

        If scTyp09 = *blanks and scLvl09 > *Zeros;
           erTyp09 = *on;
           *in90 = *On;
        Endif;

        If scTyp10 = *blanks and scLvl10 > *Zeros;
           erTyp10 = *on;
           *in90 = *On;
        Endif;

        If scTyp01 <> *blanks and scLvl01 = *Zeros;
           erLvl01 = *on;
           *in90 = *On;
        Endif;

        If scTyp02 <> *blanks and scLvl02 = *Zeros;
           erLvl02 = *on;
           *in90 = *On;
        Endif;

        If scTyp03 <> *blanks and scLvl03 = *Zeros;
           erLvl03 = *on;
           *in90 = *On;
        Endif;

        If scTyp04 <> *blanks and scLvl04 = *Zeros;
           erLvl04 = *on;
           *in90 = *On;
        Endif;

        If scTyp05 <> *blanks and scLvl05 = *Zeros;
           erLvl05 = *on;
           *in90 = *On;
        Endif;

        If scTyp06 <> *blanks and scLvl06 = *Zeros;
           erLvl06 = *on;
           *in90 = *On;
        Endif;

        If scTyp07 <> *blanks and scLvl07 = *Zeros;
           erLvl07 = *on;
           *in90 = *On;
        Endif;

        If scTyp08 <> *blanks and scLvl08 = *Zeros;
           erLvl08 = *on;
           *in90 = *On;
        Endif;

        If scTyp09 <> *blanks and scLvl09 = *Zeros;
           erLvl09 = *on;
           *in90 = *On;
        Endif;

        If scTyp10 <> *blanks and scLvl10 = *Zeros;
           erLvl10 = *on;
           *in90 = *On;
        Endif;
       //*** End   Add 07/29/20 *******************************************
      /End-Free
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     PSKEY         SCKEY
     C                   MOVEL     PSDESC        SCDESC
     C                   MOVEL     PSSRT         SCSRT
     C                   MOVEL     PSTYPE        SCTYPE
     C                   MOVEL     PSSTAT        SCSTAT
      /Free
       //*** Begin Add *** 07/29/20 ******************************************
       PStyp1 = scTyp01;
       PStyp2 = scTyp02;
       PStyp3 = scTyp03;
       PStyp4 = scTyp04;
       PStyp5 = scTyp05;
       PStyp6 = scTyp06;
       PStyp7 = scTyp07;
       PStyp8 = scTyp08;
       PStyp9 = scTyp09;
       PStyp10 = scTyp10;

       PSlvl1 = scLvl01;
       PSlvl2 = scLvl02;
       PSlvl3 = scLvl03;
       PSlvl4 = scLvl04;
       PSlvl5 = scLvl05;
       PSlvl6 = scLvl06;
       PSlvl7 = scLvl07;
       PSlvl8 = scLvl08;
       PSlvl9 = scLvl09;
       PSlvl10 = scLvl10;
       //*** End   Add *** 07/29/20 ******************************************
       /End-Free
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCKEY
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCSRT
     C                   MOVE      *BLANKS       SCTYPE
     C                   MOVEL     'A'           SCSTAT
      /Free
       //*** Begin Add *** 07/29/20 ******************************************
       scTyp01 = *Blanks;
       scTyp02 = *Blanks;
       scTyp03 = *Blanks;
       scTyp04 = *Blanks;
       scTyp05 = *Blanks;
       scTyp06 = *Blanks;
       scTyp07 = *Blanks;
       scTyp08 = *Blanks;
       scTyp09 = *Blanks;
       scTyp10 = *Blanks;
       sclVL01 = *Zeros;
       sclVL02 = *Zeros;
       sclVL03 = *Zeros;
       sclVL04 = *Zeros;
       sclVL05 = *Zeros;
       sclVL06 = *Zeros;
       sclVL07 = *Zeros;
       sclVL08 = *Zeros;
       sclVL09 = *Zeros;
       sclVL10 = *Zeros;

       //*** End   Add *** 07/29/20 ******************************************
      /End-Free
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         PSCO#
     C                   MOVEL     SCKEY         PSKEY
     C                   MOVEL     SCDESC        PSDESC
     C                   MOVEL     SCSRT         PSSRT
     C                   MOVEL     SCTYPE        PSTYPE
     C                   MOVEL     SCSTAT        PSSTAT
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
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
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
      /Free
       //*** Begin Add 07/29/20 *******************************************
        erTyp01 = *Off;
        erTyp02 = *Off;
        erTyp03 = *Off;
        erTyp04 = *Off;
        erTyp05 = *Off;
        erTyp06 = *Off;
        erTyp07 = *Off;
        erTyp08 = *Off;
        erTyp09 = *Off;
        erTyp10 = *Off;
        erLvl01 = *Off;
        erLvl02 = *Off;
        erLvl03 = *Off;
        erLvl04 = *Off;
        erLvl05 = *Off;
        erLvl06 = *Off;
        erLvl07 = *Off;
        erLvl08 = *Off;
        erLvl09 = *Off;
        erLvl10 = *Off;
       //*** End   Add 07/29/20 *******************************************
      /End-Free
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
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
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        PSUSER
     C                   MOVEL     Q1PGM         PSPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PSDT
     C*
     C                   MOVEL     DSTIME        PSTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        PSUSER
     C                   MOVEL     Q1PGM         PSPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PSDT
     C*
     C                   MOVEL     DSTIME        PSTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WTPSP
     C*
     C                   ENDSR
