     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W1555R02 - Approved States Information
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: W1555R - Work With Approved States                 *
     H*                                                               *
     H* CALLS TO:  SY601R - Search Valid State.                       *
     H*                                                               *
     H*                                                               *
     H* CREATED:                                                      *
     H*                                                               *
     H* 11/01/07 C:184 PROJECT WC1530                                 *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
      *                                                               *
      * 12/12/19 C:291 Project: WC2693                                *
      *                Add Expiration date on the screen.             *
      *                Change Adoption date field heading to          *
      *                "Approval/Exclusion date".                     *
      *                Moved "Approved or Excluded state" field after *
      *                the State field.                               *
      *                Add "2=Change" option.                         *
      *                Display file Change: Change Read Only indicator*
      *                on SCDT(State) field from 09 to N40.           *
      *                                                               *
      * 01/09/18 C:62  Project: 133WQH                                *
      *                Add 'Adoption Date'                            *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* GENERAL                                                       *
     H*  60       GENERAL READ/CHAIN                                  *
     H*                                                               *
     H* SCR01                                                         *
     H*  71       Invalid State.                                      *
     H*  72       Duplicate record not allowed                        *
     H*  73       "Approved State" value                              *
     H*  81       Valid Values are "A" or "E"                         *
     H*****************************************************************
      *
      *
     FW1555F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
      *  Physical file definition (for maintaining record)
     FWTESP     UF A E             DISK
     F                                     RENAME(PES01:DBRCDF)
     FWTESL     IF   E           K DISK
     F                                     RENAME(PES01:PES02)
      *  State
     FSTSTL1    IF   E           K DISK
     FSTSTL3    IF   E           K DISK
     F                                     RENAME(PST01:PST03)
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PST02)
      *
      *
      *  ENDORSEMENT TABLE
     FWTENP     IF   E             DISK
      *
      *
     D***PY SCOPYMT01I
      *
      *================================================================
      *  /COPY required constants
      *================================================================
      /COPY SCOPYMT01D
     D OPTNM           C                   CONST('Approved/Excluded St-
     D                                     ates Information')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
      *
      *** Beg Add ***** 12/12/19 ******************************
     DxxExDt           S              7  0
     D##datein         S              8  0
     D##fromfmt        S              8
     D##tofmt          S              8
     D##dateout        S             10
     D##err            S              1

     DSRCVTDAT1        PR                  Extpgm('SRCVTDAT')
     D##datein                        8  0
     D##fromfmt                       8
     D##tofmt                         8
     D##dateout                      10
     D##err                           1
      *** End Add ***** 12/12/19 ******************************
      *
      *
      *================================================================
      *  PLIST definitions
      *================================================================

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          WTESP
     C                   PARM                    @@RRN1            9 0          WTENP
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *
      *
      *================================================================
      *  KLIST definitions
      *================================================================
      *
      *  State
      *
     C     STKY          KLIST
     C                   KFLD                    KKKEY
      *
      *  Approved States
      *
     C     ESKY          KLIST
     C                   KFLD                    ENCO#
     C                   KFLD                    ENFORM
     C                   KFLD                    SCST
      *
      *
      *  Approved or Excluded state
      *
     C     ESKY1         KLIST
     C                   KFLD                    ENCO#
     C                   KFLD                    ENFORM
      *
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    STKEY         KKKEY
     C*** Begin Add *** 01/09/18 *********************************
     C     *LIKE         DEFINE    ENaDte        xxAdte
     C*** End   Add *** 01/09/18 *********************************
      *
      /COPY SCOPYMT01C
      *
      *================================================================
      * DLTPRE - Pre-processing before delete occurs
      *================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
      *
      *
      *
      *================================================================
      * DSPLYR - Processing after displaying information
      *================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
      *
      *
      *
      *================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *================================================================
     C     ERRCK1        BEGSR
      *
      * State code must be valid
      *
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  SCST          KKKEY
     C     STKY          CHAIN     PST01                              60
     C  N60              MOVEL(P)  STNAME        SCSTNM
     C   60              MOVEL(P)  *BLANKS       SCSTNM
     C   60              SETON                                        71  90
     C                   ENDIF
      *
      * Approved State code must be valid
      *
     C     *IN90         IFEQ      *OFF
     C*
     C     STKY          CHAIN     PST03                              60
     C* N60                MOVELSTNAME    SCSTNM    P
     C*  60                MOVEL*BLANKS   SCSTNM    P
     C   60              SETON                                        73  90
     C                   ENDIF
      *
      * State code must unique for employer (no duplicate records)
      *
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C     ESKY          CHAIN     PES02                              60
     C  N60              SETON                                        72  90
     C                   ENDIF
     C*** Begin Add *** 01/09/18 *********************************
     C                   Eval      mgAdte = ' '
     C                   Eval      erAdte = *Off
      *** Beg Add ***** 12/12/19 ************************************
     C                   Clear                   xxAdte
      *** End Add ***** 12/12/19 ************************************
      *
     ** Adoption date must be valid if entered
     C     *IN90         IFEQ      *OFF
     C     scAdte        IfNE      0
     C                   Z-ADD     SCADTE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C     SRERR         IFEQ      *ON
     C                   Eval      mgAdte = 'Invalid date.'
     C                   Eval      erAdte = *On
     C                   Eval      *in90  = *On
     C                   Else
     C*
     C* Adoption date must be greater than or equal to the edition date
     C* if entered

     C                   Z-ADD     scAdte        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         xxAdte

     C                   If        xxAdte < ENedDt
     C                   Eval      mgAdte = 'Adoption date can''t be less '  +
     C                             'than the Edition date.'
     C                   Eval      erAdte = *On
     C                   Eval      *in90  = *On
     C                   EndIf
     C                   EndIf                                                  SRERR=*ON
      *
     C                   EndIf                                                  SRERR=*ON
     C                   EndIf                                                  *IN90 = *OFF
     C*** End   Add *** 01/09/18 *******************************************
     C*** End   Add *** 01/09/18 *********************************
      *
      *
      *Approved or Excluded state (A/E) is a required field when entering the
      *first WTESL record for an endorsement.
      *
     C     *IN90         IFEQ      *OFF
     C*
     C     ESKY1         CHAIN     PES02                              60
     C     *IN60         IFEQ      *ON
     C     SCAPEX        ANDEQ     ' '
     C                   SETON                                        81  90
     C                   ENDIF
     C                   ENDIF
     C     *IN90         IFEQ      *OFF
     C*
     C     SCAPEX        IFNE      'A'
     C     SCAPEX        ANDNE     'E'
     C                   SETON                                        81  90
     C                   ELSE
     C                   MOVE      SCAPEX        #APEX
     C                   SETON                                            11
     C                   ENDIF
     C                   ENDIF
     C*
      //*** Beg Add ***** 12/12/19 *********************************
      /Free
        ##fromfmt = *Blanks;
        ##tofmt = '*CYMD';
        ##dateout = *Blanks ;
        ##err = *Blanks ;
        Clear xxExDt;

        //Expiration date must be valid
        mgExdt = ' ';
        erExdt = *Off;
        If scExdt <> 0;
           ##datein = scExdt ;
           SRCVTDAT1(##datein : ##fromfmt : ##tofmt : ##dateout : ##err) ;
           If ##err = '1' ;
              mgExDt = 'Invalid date.';
              erExDt = *On;
              *in90  = *On;
           Else;
              xxExdt = %dec(##dateout:7:0);
           Endif;
        Endif;

        //Expiration date cannot be less than Approval/Exclusion date.
        If (erAdte <> *On and erExdt <> *On) and
         xxAdte <> 0 and xxExdt <> 0 and xxExdt < xxAdte;
           mgExdt = 'Expiration date cannot be less than Approval/Exclusion -
                     date.';
            erExdt = *On;
            *in90  = *On;
        EndIf;
      /End-Free
      //*** End Add ***** 12/12/19 *********************************
     C                   ENDSR                                                  ERRCK1
      *
      *
      *
      *================================================================
      * ERRCK2 - Error checking for srceen 2 (SCR02)
      *================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
      *
      *
      *
      *================================================================
      * ERRCK3 - Error checking for srceen 3 (SCR03)
      *================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR                                                  ERRCK3
      *
      *
      *
      *================================================================
      * KJDO - Searches
      *================================================================
     C     KJDO          BEGSR
      *
      * State
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCST'
     C                   CALL      'SY601R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      *ZERO         ##RRN             9 0
      *
     C     ##RRN         IFEQ      *ZERO
      *
     C                   ELSE
      *
     C     ##RRN         CHAIN     PST02                              60
     C                   MOVEL(P)  STKEY         SCST
     C                   MOVEL(P)  STNAME        SCSTNM
     C                   ENDIF                                                  ##RRN
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  KJDO
      *
      *
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   MOVE      ESST          SCST
     C                   MOVE      ESAPEX        SCAPEX
      *
     C     SCAPEX        IFEQ      ' '
     C                   MOVE      #APEX         SCAPEX
     C                   ENDIF
      *
     C                   MOVEL(P)  SCST          KKKEY
     C     STKY          CHAIN     PST01                              60
     C  N60              MOVEL(P)  STNAME        SCSTNM
     C   60              MOVEL(P)  *BLANKS       SCSTNM
     C*** Begin Add *** 01/09/18 **************************************
     C                   Z-ADD     ESADTE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCADTE
     C*** End   Add *** 01/09/18 **************************************
     c*** End   Add *** 01/09/18 ************************************
      //*** Beg Add ***** 12/12/19 *********************************
      /Free
         ##fromfmt = '*CYMD';
         ##tofmt = ' ';
         scExdt = 0;
         If ESExdt <> 0;
            ##datein = ESExdt ;
            SRCvtDat1(##datein : ##fromfmt : ##tofmt : ##dateout : ##err) ;
            If ##err = '0' ;
               scExdt = %dec(##dateout:8:0);
            Endif;
         Endif;
      /End-Free
      //*** End Add ***** 12/12/19 *********************************
      *
     C                   ENDSR                                                  MVDBSC
      *
      *
      *
      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     MVSCDB        BEGSR
      *
     C                   Z-ADD     ENCO#         ESCO#
     C                   MOVE      ENFORM        ESFORM
      *
     C                   MOVE      SCST          ESST
     C                   MOVE      SCAPEX        ESAPEX
     C*** Begin Add *** 01/09/18 **************************************
     C                   Z-ADD     SCADTE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ESADTE
     C*** End   Add *** 01/09/18 **************************************
      //*** Beg Add ***** 12/12/19 *********************************
      /Free
        If scExdt <> 0;
           ##fromfmt = *Blanks;
           ##tofmt = '*CYMD';
           ##err = *Blanks ;
           ##datein = scExdt ;
           SRCvtDat1(##datein : ##fromfmt : ##tofmt : ##dateout : ##err) ;
           If ##err = '0' ;
              ESexdt = %dec(##dateout:7:0);
           Endif;
        Else;
           ESexdt = 0;
        Endif;
      /End-Free
      /End-Free
      //*** End Add ***** 12/12/19 *********************************
      *
     C                   ENDSR                                                  MVSCDB
      *
      *
      *
      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
     C     OTHERR        BEGSR
     C                   ENDSR                                                  OTHERR
      *
      *
      *
      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
     C     OTHFKS        BEGSR
      *
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHFKS
      *
      *
      *
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
      *
      *
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
     C*** Begin Add *** 01/09/18 *********************************
     C                   Eval      mgAdte = ' '
     C                   Eval      erAdte = *Off
     C*** End   Add *** 01/09/18 *********************************
      /Free
        mgExdt = ' ';
        erExdt = *Off;
      /End-Free
     C     @@MODE        COMP      'A'                                    40    ADD
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    11    INQUIRY
      *
     C                   MOVE      *BLANK        #APEX             1
     C                   MOVE      *OFF          *IN41
     C*
     C     @@RRN1        CHAIN     PEN01                              60
     C   60              SETON                                        LR
     C   60              RETURN
      *
     C     ENFORM        CAT       ENDESC:2      SCFORD
     C                   MOVE      ENFND         SCFND
      *
      *Approved or Excluded state (A/E) is a required field when entering the
      *first WTESL record for an endorsement.
      *
     C*
     C     ESKY1         CHAIN     PES02                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      ESAPEX        #APEX
     C                   SETON                                            11
     C                   ENDIF
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     PREOTH        BEGSR
     C                   ENDSR                                                  PREOTH
      *
      *
      *
      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     C     UPDATR        BEGSR
      **** Beg Chg ***** 12/12/19 *********************************
      ***  @@RRN1        CHAIN     DBRCDF                             99
     C     @@RRN         CHAIN     DBRCDF                             99
      **** End Chg ***** 12/12/19 *********************************
      *
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        ESUSR
     C                   MOVEL     Q1PGM         ESPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ESDT
     C                   MOVEL     DSTIME        ESTIM
      *
     C                   UPDATE    DBRCDF
     C                   ENDIF
      *
     C                   ENDSR                                                  UPDATR
      *
      *
      *
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     WRITER        BEGSR
      *
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        ESIUSR
     C                   MOVEL     Q1USER        ESUSR
     C                   MOVEL     Q1PGM         ESIPGM
     C                   MOVEL     Q1PGM         ESPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ESIDT
     C                   MOVEL     SRDTO         ESDT
     C                   MOVEL     DSTIME        ESITIM
     C                   MOVEL     DSTIME        ESTIM
      *
     C                   WRITE     DBRCDF
     C                   FEOD      WTESP                                        WRITER
      *
     C                   ENDSR
      *
      *
      *
