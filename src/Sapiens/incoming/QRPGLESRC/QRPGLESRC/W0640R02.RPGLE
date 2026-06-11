     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE') bnddir('WTBNDDIR')
     H/define CONVERT
     H*
     H****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/07/21  C:33  Project: 129WWK                               *
     H*                 Translate client codes.                       *
     H*                                                               *
     H* 10/14/14 C:280   PROJECT 114WPE                               *
     H*          C:33    Agent Commissions Modifications              *
     H*                                                               *
     H* 02/08/02 C:61  SUPPORT WA099 31198                            *
     H*               "Agency Billed" should not be required field.   *
     H*               Screen change only.                             *
     H*                                                              *
     H*  07/19/99  C:33  PROJECT: WC899                              *
     H*                  ADD AGENCY BILL                             *
     H****************************************************************
     F*
     FW0640F02  CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F*
     F*  Physical file definition (for maintaining record)
     F*
     FWMARP     UF A E             DISK
     F                                     RENAME(PAR01:DBRCDF)
     F                                     INFDS(DBFDS)
     F*
     F*** Agent Rate FIle
     F*
     FWMFNP     IF   E             DISK
     F*
     F*** Fund Master File
     F*
     FWMARL     IF   E           K DISK
     F*
     F*** Agent Rate File by CO#/FND/RT#/EFDT
     F*
     FSSYLF     IF   E           K DISK
     F*
     F*** Applicate System File by Key
     F*
     F*
      /COPY SCOPYMT01D
     D*
     D #ofARSec        c                   10
     D*
     D intCount        s              2  0
     D*
     D* Data Base Security Type 1-10
     D*
     D                 ds
     D dsArSecTyp              1     10    dim(#ofARSec   )
     D  ARsectyp01             1      1
     D  ARsectyp02             2      2
     D  ARsectyp03             3      3
     D  ARsectyp04             4      4
     D  ARsectyp05             5      5
     D  ARsectyp06             6      6
     D  ARsectyp07             7      7
     D  ARsectyp08             8      8
     D  ARsectyp09             9      9
     D  ARsectyp10            10     10
     D*
     D* Data Base Security Level 1-10
     D*
     D                 ds
     D dsArSecLvl              1     30  0 dim(#ofARSec   )
     D  ARseclvl01             1      3  0
     D  ARseclvl02             4      6  0
     D  ARseclvl03             7      9  0
     D  ARseclvl04            10     12  0
     D  ARseclvl05            13     15  0
     D  ARseclvl06            16     18  0
     D  ARseclvl07            19     21  0
     D  ARseclvl08            22     24  0
     D  ARseclvl09            25     27  0
     D  ARseclvl10            28     30  0
     D*
     D* Screen Security Type 1-10
     D*
     D                 ds
     D dsScSecTyp              1     10    dim(#ofARSec   )
     D  scSECTYP01             1      1
     D  scSECTYP02             2      2
     D  scSECTYP03             3      3
     D  scSECTYP04             4      4
     D  scSECTYP05             5      5
     D  scSECTYP06             6      6
     D  scSECTYP07             7      7
     D  scSECTYP08             8      8
     D  scSECTYP09             9      9
     D  scSECTYP10            10     10
     D*
     D* Screen Security Level 1-10
     D*
     D                 ds
     D dsScSecLvl              1     30  0 dim(#ofARSec   )
     D  scSECLVL01             1      3  0
     D  scSECLVL02             4      6  0
     d  scSECLVL03             7      9  0
     d  scSECLVL04            10     12  0
     d  scSECLVL05            13     15  0
     d  scSECLVL06            16     18  0
     d  scSECLVL07            19     21  0
     d  scSECLVL08            22     24  0
     d  scSECLVL09            25     27  0
     d  scSECLVL10            28     30  0
     D*
     D* Security Type Error Indicator
     D*
     D                 ds
     D dsErSecTyp              1     10    dim(#ofARSec   )
     D  erSECTYP01             1      1
     D  erSECTYP02             2      2
     D  erSECTYP03             3      3
     D  erSECTYP04             4      4
     D  erSECTYP05             5      5
     D  erSECTYP06             6      6
     D  erSECTYP07             7      7
     D  erSECTYP08             8      8
     D  erSECTYP09             9      9
     D  erSECTYP10            10     10
     D*
     D* Security Level Error Indicator
     D*
     d                 ds
     d dsErSecLvl              1     10    dim(#ofARSec   )
     d  erSECLVL01             1      1
     d  erSECLVL02             2      2
     d  erSECLVL03             3      3
     d  erSECLVL04             4      4
     d  erSECLVL05             5      5
     d  erSECLVL06             6      6
     d  erSECLVL07             7      7
     d  erSECLVL08             8      8
     d  erSECLVL09             9      9
     d  erSECLVL10            10     10
     D*
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Agent Rate Master-
     D                                      Information')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
     D*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
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
     C     ARKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCRT#
     C                   KFLD                    KKEFDT
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    AREFDT        KKEFDT
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     ARSTAT        IFEQ      'I'
     C                   MOVE      'A'           ARSTAT
     C                   ELSE
     C                   MOVE      'I'           ARSTAT
     C                   ENDIF                                                  ARSTAT
     C*
     C                   MOVEL     Q1USER        ARUSER
     C                   MOVEL     Q1PGM         ARPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   MOVEL     DSTIME        ARTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN60
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
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C* RATE MUST BE GREATER THAN ZERO (IF IN ADD MODE)
     C*
     C   40
     CANN90SCRT#         IFLE      *ZERO
     C                   SETON                                        75  90
     C                   ENDIF                                                  ARRT#P
     C*
     C* EFFECTIVE DATE MUST BE VALID
     C*
     C     *IN40         IFEQ      *ON
     C     *IN90         ANDEQ     *OFF
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C   40
     CANN90SRERR         IFEQ      '1'
     C*
     C                   SETON                                        7790
     C*
     C                   ENDIF                                                  SRERR
     C*
     C                   ENDIF                                                  *IN40,90
     C*
     C* CHECK TO BE SURE RATE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C   40
     CANN90ARKY          CHAIN     PAR01                              60
     C   40
     CANN90
     CANN60              SETON                                        76  90
     C*
     C* RATE DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        78  90
     C                   ENDIF                                                  SCDESC
     C*
     C*
     C  N90SCPLAN        IFNE      'A'
     C     SCPLAN        ANDNE     'W'
     C     SCPLAN        ANDNE     *BLANK
     C                   SETON                                        79  90
     C                   ENDIF                                                  SCPLAN<>A/*BLANK
     C*
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90ARDT          IFGT      SAVEDT
     C     ARDT          OREQ      SAVEDT
     C     ARTIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF
     C*
     C*
      /Free

        If not *in90;

          For intCount = 1 to #ofARSec;

             // Security Levels and Types are mutually inclusive
             If dsScSecTyp(intCount) = *blanks and dsScSecLvl(intCount) > 0
              or dsScSecTyp(intCount) <> *blanks and dsScSecLvl(intCount) = 0;
                dsErSecTyp(intCount) = *on;
                dsErSecLvl(intCount) = *on;
                *IN90 = *on;
             Endif;

          Endfor;

        Endif;

      /end-free
     C*
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK1
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
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C   40              MOVEL     'A'           ARSTAT
     C*
     C                   Z-ADD     ARRT#         SCRT#
     C                   MOVEL     ARDESC        SCDESC
     C                   MOVEL     ARSTAT        SCSTAT
     C                   MOVEL     ARNWRT        SCNWRT
     C                   MOVEL     ARRNRT        SCRNRT
     C*
     C                   Z-ADD     AREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C***  Begin Add  ***  01/07/21  ***********************************
      /Free
       Select;
       When cltId = '129' and ARplan = 'B';
          ARplan = 'W';
       When cltId = '129' and ARplan = 'D';
          ARplan = ' ';
       Endsl;
      /End-free
     C***  End   Add  ***  01/07/21  ***********************************
     C*
     C                   MOVEL     ARPLAN        SCPLAN
     C*
      /Free

       scExpConst = ARexpConst;
       scPlan     = ARplan;
       dsScSecTyp = dsArSecTyp;
       dsScSecLvl = dsArSecLvl;

      /end-free
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     FNCO#         ARCO#
     C                   Z-ADD     SCFND         ARFND
     C                   Z-ADD     SCRT#         ARRT#
     C                   MOVEL     SCDESC        ARDESC
     C                   MOVEL     SCNWRT        ARNWRT
     C                   MOVEL     SCRNRT        ARRNRT
     C                   MOVEL     SCSTAT        ARSTAT
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AREFDT
     C*
     C                   MOVEL     SCPLAN        ARPLAN
     C*
      /Free
       ARexpConst = scExpConst;
       ARplan     = scPlan;
       dsArSecTyp = dsScSecTyp;
       dsArSecLvl = dsScSecLvl;
      /End-Free
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
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
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
     C                   CLEAR                   WCFL15
     C     'WC'          CHAIN     SWC01                              60
     C     WCFL15        COMP      'Y'                                    96
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C* GET GROUP INFORMATION
     C*
     C     @@RRN2        CHAIN     WMFNP                              60
     C*
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
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
     C                   MOVEL     Q1USER        ARUSER
     C                   MOVEL     Q1PGM         ARPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   MOVEL     DSTIME        ARTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF                                                  *IN99
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  UPDATR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        ARUSER
     C                   MOVEL     Q1PGM         ARPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   MOVEL     DSTIME        ARTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMARP
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  WRITER
