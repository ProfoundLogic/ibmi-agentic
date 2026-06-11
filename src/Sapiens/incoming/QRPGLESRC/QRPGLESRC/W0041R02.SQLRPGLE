     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) bnddir('WTBNDDIR')
     H/define CONVERT
     H*
     H/TITLE W0041R02 - AGENT ASSIGNMENT INFORMATION
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE AGENT ASSIGNMENT FILE.   *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0041R - WORK WITH AGENT ASSIGNMENTS                       *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    W4150R - QUOTE STATUS                                      *
     H*    SC010R - DARTS                                             *
     H*                                                               *
     H*  CREATED: 12/01/97  PROGRAMMER: C:33  PROJECT: WC683          *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/25/19 C:33  Project: SY390                                 *
     H*                Add Portal Defaul flag                         *
     H*                                                               *
     H* 12/26/14 C:283 Support WA099 87243                            *
     H*                1) Use the Default Group when restriction      *
     H*                    flag is set to 'Y' and nothing is in the   *
     H*                    restriction table.                         *
     H*                2) When only single entry is there in the      *
     H*                    Group restriction table, then use the      *
     H*                    proper groups to set the Group Restrcition.*
     H*                                                               *
     H* 09/19/13 C:283 Support WA147 83464                            *
     H*                Correct the Group Restrictions handling if     *
     H*                there are multiple groups restricted.          *
     H*                                                               *
     H* 05/04/11 C:257 SUPPORT 75908                                  *
     H*                Call SA008R to check for group restriction.   *
     H*                                                               *
     H* 01/02/02 C:136 PROJECT WC1023                                 *
     H*                Add agent federal name.                        *
     H*                                                               *
     H* 10/13/00 C:61  PROJECT WC914                                  *
     H*              - Do not load SCFND if called in copy mode='P'.  *
     H*                                                               *
     H*  08/16/99  C:118 PROJECT: WC577                               *
     H*                  ADDED ERROR MESSAGE ON NON-SEARCHABLE FIELDS.*
     H*                  MODIFIED CURSOR POSITIONING.                 *
     H*                                                               *
     H*  02/23/99  C:33  PROJECT: 48WPC                               *
     H*                  GIVE A WARNING MSG TO USER INSTEAD OF AN     *
     H*                  ERROR WHEN THE THERE IS AN ACTIVE AGENT HIST *
     H*                  RECORD OUTSIDE OF THE DATE RANGE.            *
     H*                                                               *
     H*  06/07/98  C:33  PROJECT: WC577                               *
     H*                  DO NOT ALLOW EFF/CX DATES TO HAVE ACTIVE     *
     H*                  ACCOUNTS OUTSIDE OF THIER RANGE              *
     H*                                                               *
     H*  04/23/98  C:33  PROJECT: WC683                               *
     H*                 -ADD COMMITMENT AMOUNT/DATE                   *
     H*                 -REFORMAT DATES WHEN F10 IS PRESSED           *
     H*                 -CORRECT PC WHEN AND F10 SELECTION IS MADE    *
     H*                                                               *
     H*  02/04/98  C:33  PROJECT: WC683                               *
     H*                  ADD F14=DARTS                                *
     H*****************************************************************
     FW0041F02  CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F*
     F*  Physical file definition (for maintaining record)
     FWMAAP     UF A E             DISK
     F                                     RENAME(PAA01:DBRCDF)
     FWMAGP     IF   E             DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDSF)
     FWMAAL     IF   E           K DISK
     F                                     INFDS(DBFDSA)
     FWMAAL1    IF   E           K DISK
     F                                     RENAME(PAA01:PAA02)
     FWMAHL3    IF   E           K DISK
     FWMCPL     IF   E           K DISK
     FWAQTL3    UF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)
     FWMARL     IF   E           K DISK
     FWMARP     IF   E             DISK
     F                                     RENAME(PAR01:PAR02)
     Fwmfnp     if   e             disk
     F                                     Rename(PFN01:PFN02)
      /COPY SCOPYMT01D
     D*
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D                 DS
     D  QCKEY                  1     40
     D  QCFEIN                 1      9  0
     D  QCMOD                 10     12  0
     D count           s              3P 0
       //***  Begin Add  ***  03/25/19  ***********************************
     DdecFnd           s                   like(AAfnd) inz
       //***  End   Add  ***  03/25/19  ***********************************
     D fndary          s              3P 0 dim(999)
     D savFnd          s              3P 0 inz(000)
       //***  Begin Add  ***  03/25/19  ***********************************
     DstrPortalName    s             40    varying inz
       //***  End   Add  ***  03/25/19  ***********************************
     D ##RCDS          s              1N
     D ##APPL          s              1A
     D ##AUTH          s              1A

     D fndRest         pr                  extpgm('SA008R')
     D ##CO#                          3  0
     D ##USR                         10
     D ##APPL                         1
     D ##RCDS                         1
     D ##AUTH                         1
     D ##FND                          3  0

       //***  Begin Add  ***  03/25/19  ***********************************
     D/copy sPrModule
       //***  End   Add  ***  03/25/19  ***********************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Agent Assignments')
     D COMAPI          C                   CONST('WA001R')
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
     C                   PARM                    @@RRN             9 0          AGENT ASSIGNMENT
     C                   PARM                    @@RRN2            9 0          AGENT MASTER
     C                   PARM                    @@PGM            10
     C*
     C*  NOTE: @@PGM is the calling program (only used in -ADD- mode).
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
     C                   PARM                    ##XCO#            1
     C                   PARM                    ##XFND            1
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    ##CO#
     C*
     C*  KEY LIST FOR THE GROUP
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C                   KFLD                    SCFND
     C*
     C     AAKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SVFEIN
     C                   KFLD                    SVMOD
     C                   KFLD                    SCFND
     C*
     C     AAKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCAGT#
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SVAGT#
     C*
     C*  KEY LIST FOR CHAPTER
     C*
     C     CPKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCCHAP
     C*
     C*  AGENT QUOTE FILE
     C*
     C     QTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SVAGT#
     C*
     C*  AGENT RATE FILE
     C*
     C     ARKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCDRT#
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    AAAGT#        SVAGT#
     C     *LIKE         DEFINE    AAFEIN        SVFEIN
     C     *LIKE         DEFINE    AAMOD         SVMOD
     C     *LIKE         DEFINE    AAEFDT        XXDAT1
     C     *LIKE         DEFINE    AACNDT        XXDAT2
     C     *LIKE         DEFINE    CPCHAP        ##CHAP
     C     *LIKE         DEFINE    SCEFDT        SVEFDT
     C     *LIKE         DEFINE    SCCNDT        SVCNDT
     C     *LIKE         DEFINE    AAEFDT        X7EFDT
     C     *LIKE         DEFINE    AACNDT        X7CNDT

       //***  Begin Add  ***  03/25/19  ***********************************
      /Free
       Exec Sql Set Option Commit = *None;
      /End-free
       //***  End   Add  ***  03/25/19  ***********************************
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
     C     AASTAT        IFEQ      'I'
     C                   MOVE      'A'           AASTAT
     C                   ELSE
     C                   MOVE      'I'           AASTAT
     C                   ENDIF                                                  AASTAT=I
     C*
     C                   MOVEL(P)  Q1USER        AAUSER
     C                   MOVEL(P)  Q1PGM         AAPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AADT
     C*
     C                   MOVEL     DSTIME        AATIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  CHGSTS
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C                   Move      '0'           *IN69
B01  C     ##xfnd        Ifeq      'Y'
     C     *IN40         Andeq     *On
     C*
      /Free
       ##rcds = *Off;
       If %Lookup(SCfnd : fndAry) = *Zeros;
          ##rcds = *On;
       Endif;
      /End-Free
     C*
     C     ##rcds        Ifeq      *On
     C                   Move      *On           *IN90
     C                   Move      *On           *IN69
     C                   Endif
      *
     C                   Endif
     C  N90*IN40         IFEQ      *ON
     C*
     C*  CHECK FOR VALID GROUP NUMBER
     C*
     C     FNKY          CHAIN     WMFNL                              60
     C*
     C     *IN60         IFEQ      *ON
     C     FNSTAT        OREQ      'I'
     C                   SETON                                        71  90
     C                   ENDIF                                                  *IN60=*ON/FNSTAT=I
     C*
     C*  BE SURE THE FEIN/MOD HAS NOT BEEN PREVIOUSLY BEEN ASSIGNED TO
     C*  TO THIS GROUP
     C*
     C  N90AAKY          CHAIN     PAA01                              60
     C  N90
     CANN60              SETON                                        72  90
     C*
     C  N90              MOVEL(P)  FNSHNM        SCFNAM
     C*
     C                   ENDIF                                                  *IN40=*ON

       //***  Begin Add  ***  03/25/19  ***********************************
      /Free
       // Check for portal default flag set for another group
       erPortDft = *off;
       If not *in90 and scPortDft = 'Y';
          Reset decFnd;
          Exec sql
           select AAfnd into :decFnd
           from WMAAP
            where AAco# = :##co#
             and AAfein = :scFein
             and AAmod = :scMod
             and AAfnd <> :scFnd
             and AAportDft = 'Y';
          If decFnd <> 0;
             *in90 = *on;
             erPortDft = *on;
             msPortDft = 'Group ' + %editc(decFnd:'Z') +
              ' is already defined as the default group.';
          Endif;
       Endif;
      /End-free
       //***  End   Add  ***  03/25/19  ***********************************
     C*
     C*  AGENT NUMBER MAY NOT BE ZERO
     C*
     C  N90SCAGT#        IFLE      *ZERO
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCAGT#<=*ZERO
     C*
     C*  IF AT LEAST ONE ACTIVE ACCOUNT EXISTS FOR THIS AGENT, SEND AN
     C*  ERROR MESSAGE IF THE USER TRIES TO CHANGE THE AGENT NUMBER.
     C*
     C  N90SVAGT#        IFNE      SCAGT#
     C*
     C     AHKY          CHAIN     PAH01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     AHSTAT        IFEQ      'A'
     C                   SETON                                        74  90
     C                   LEAVE
     C                   ENDIF                                                  AHSTAT=A
     C*
     C     AHKY          READE     PAH01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  SVAGT#<>SCAGT#
     C*
     C*  CHECK FOR AGENT NUMBER ALREADY ASSIGNED
     C*
     C  N90AAKY2         CHAIN     PAA02                              60
     C*
     C  N90*IN60         IFEQ      *OFF
     C*
     C     AAFEIN        IFNE      SCFEIN
     C     AAFEIN        OREQ      SCFEIN
     C     AAMOD         ANDNE     SCMOD
     C*
     C     AASTAT        IFNE      'I'
     C                   SETON                                        75  90
     C                   ELSE
     C                   Z-ADD     AAFEIN        SVFEIN
     C                   Z-ADD     AAMOD         SVMOD
     C                   ENDIF                                                  AASTAT<>I
     C*
     C                   ENDIF                                                  AAFEIN<>SCFEIN
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   CLEAR                   XXDAT1
     C                   CLEAR                   XXDAT2
     C*
     C*  VALIDATE EFFECTIVE DATE
     C*
     C     SCEFDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT1
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        79  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCEFDT<>*ZEROS
     C*
     C*  VALIDATE CANCELLATION DATE
     C*
     C     SCCNDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCCNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCNDT
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT2
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        80  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C  N90XXDAT1        IFEQ      *ZEROS
     C     XXDAT1        ORGT      XXDAT2
     C                   SETON                                        81  90
     C                   ENDIF                                                  *ZEROS=XXDAT1>XXDAT2
     C*
     C                   ENDIF                                                  SCCNDT<>*ZEROS
     C*
     C                   Z-ADD     XXDAT1        X7EFDT
     C                   Z-ADD     XXDAT2        X7CNDT
     C*
     C*  IF AT LEAST ONE ACCOUNT EXISTS OUTSIDE OF THE NEW DATE RANGES
     C*  DO NOT ALLOW THE USER TO CHANGE THE DATES.
     C*
     C  N90SVEFDT        IFNE      SCEFDT
     C     SVCNDT        ORNE      SCCNDT
     C*
     C     AHKY          CHAIN     PAH01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     *IN90         ANDEQ     *OFF
     C*
     C     AHSTAT        IFEQ      'A'
     C*
     C     X7EFDT        IFGT      AHEFDT
     C                   SETON                                        89  90
     C                   ENDIF                                                  X7EFDT>AHEFDT
     C*
     C     X7CNDT        IFNE      *ZEROS
     C     X7CNDT        ANDLT     AHEFDT
     C                   SETON                                        89  90
     C                   ENDIF                                                  X7CNDT>AHEFDT
     C*
     C                   ENDIF                                                  AHSTAT=A
     C*
     C     AHKY          READE     PAH01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   Z-ADD     SCEFDT        SVEFDT
     C                   Z-ADD     SCCNDT        SVCNDT
     C*
     C                   ENDIF                                                  SVEFDT<>SCEFDT
     C*
     C*  EDIT DEFAULT RATE NUMBER
     C*
     C  N90SCDRT#        IFNE      *ZEROS
     C     ARKY          CHAIN     PAR01                              60
     C  N90
     CAN 60              SETON                                        84  90
     C                   ENDIF                                                  SCDRT#<>*ZEROS
     C*
     C*  EDIT DEFUALT RATE EXPIRATION DATE
     C*
     C     SCEXDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        85  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C  N90SCDRT#        IFEQ      *ZEROS
     C                   SETON                                        86  90
     C                   ENDIF                                                  SCDRT#=*ZEROS
     C*
     C                   ENDIF                                                  SCEXDT=*ZEROS
     C*
     C*  VALIDATE UNDERWRITER STAFF CODE, IF ENTERED
     C*
     C  N90SCUWTR        IFNE      *BLANKS
     C     SCUWTR        CHAIN     PUP02                              60
     C  N60              MOVEL(P)  UPUSNM        SCUNAM
     C   60              SETON                                        83  90
     C                   ENDIF                                                  SCUWTR<>*BLANKS
     C*
     C*  CHAPTER MUST BE VALID AND ACTIVE, IF ENTERED
     C*
     C  N90SCCHAP        IFNE      *ZEROS
     C*
     C     CPKY          CHAIN     WMCPL                              60
     C*
     C     *IN60         IFEQ      *ON
     C     CPSTAT        OREQ      'I'
     C                   SETON                                        82  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C  N90              MOVEL(P)  CPDESC        SCCHPD
     C*
     C                   ENDIF                                                  SCCHAP<>*ZEROS
     C*
     C*  COMMITMENT AMOUNT AND DATE ARE MUTUALLY INCLUSIVE
     C*
     C  N90SCCMMT        IFEQ      *ZEROS
     C     SCCMDT        ANDNE     *ZEROS
     C     SCCMMT        ORNE      *ZEROS
     C     SCCMDT        ANDEQ     *ZEROS
     C                   SETON                                        87  90
     C                   ENDIF                                                  SCCMMT=*ZEROS<>SCCMD
     C*
     C*  VALIDATE COMMITMENT DATE
     C*
     C  N90SCCMDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCCMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCMDT
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        88  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCCMDT<>*ZEROS
     C*
     C*  PAY COMMISSION FLAG MUST BE 'Y' OR 'N'
     C*
     C  N90SCCMFL        IFNE      'Y'
     C     SCCMFL        ANDNE     'N'
     C                   SETON                                        76  90
     C                   ENDIF                                                  SCCMFL<>Y/N
     C*
     C*  MAIL FLAG MUST BE 'Y' OR 'N'
     C*
     C  N90SCMLFL        IFNE      'Y'
     C     SCMLFL        ANDNE     'N'
     C                   SETON                                        77  90
     C                   ENDIF                                                  SCMLFL<>Y/N
     C*
     C*  PROSPECT FLAG MUST BE 'Y' OR 'N'
     C*
     C  N90SCPRFL        IFNE      'Y'
     C     SCPRFL        ANDNE     'N'
     C                   SETON                                        78  90
     C                   ENDIF                                                  SCPRFL<>Y/N
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90AADT          IFGT      SAVEDT
     C     AADT          OREQ      SAVEDT
     C     AATIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  AADT>SAVEDT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C*
     C     ERRCK3        BEGSR
     C                   ENDSR                                                  ERRCK3
     C*
     C*================================================================
     C* KIDO - Processing for F9
     C*================================================================
     C*
     C     KIDO          BEGSR
     C*
     C   09              MOVE      '1'           ##CALL
     C  N09              MOVE      ' '           ##CALL
     C*
     C                   CALL      'W4150R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      @@RRN         ##RRN             9 0
     C*
     C                   ENDSR                                                  KIDO
     C*
     C*================================================================
     C* KJDO - Processing for F10
     C*================================================================
     C*
     C     KJDO          BEGSR
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCEFDT
     C                   ENDIF                                                  SRERR=*OFF
     C*
     C                   Z-ADD     SCCNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCCNDT
     C                   ENDIF                                                  SRERR=*OFF
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCEXDT
     C                   ENDIF                                                  SRERR=*OFF
     C*
     C                   Z-ADD     SCCMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCCMDT
     C                   ENDIF                                                  SRERR=*OFF
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCFND'
     C     *IN40         ANDEQ     *ON
     C*
     C                   MOVE      SCFND         #@FND
     C*
     C                   CALL      'W0010R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    #@CO#             3
     C                   PARM      *BLANKS       #@FND             3
     C*
     C     #@FND         IFNE      *BLANKS
     C*
     C                   MOVE      #@FND         SCFND
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNSHNM        SCFNAM
     C                   MOVE      *ON           *IN22
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN21
     C*
     C                   ENDIF                                                  #@FND<>*BLANKS
     C*
     C*_______________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCCHAP'
     C*
     C                   MOVE      *OFF          *IN90
     C*
     C*  CHECK FOR VALID GROUP NUMBER
     C*
     C     FNKY          CHAIN     WMFNL                              60
     C*
     C     *IN60         IFEQ      *ON
     C     FNSTAT        OREQ      'I'
     C                   SETON                                        71  90
     C                   ENDIF                                                  *IN60=*ON/FNSTAT=I
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   CALL      'W0720R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      DBRRNF        ##RRN2            9 0
     C                   PARM      *ZEROS        ##CHAP
     C*
     C     ##CHAP        IFNE      *ZEROS
     C*
     C                   Z-ADD     ##CHAP        SCCHAP
     C     CPKY          CHAIN     PCP01                              60
     C  N60              MOVEL(P)  CPDESC        SCCHPD
     C                   MOVE      *ON           *IN27
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN23
     C*
     C                   ENDIF                                                  ##CHAP<>*ZEROS
     C*
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C*_______________________________________________________________
     C*
     C*  STAFF CODES
     C*
     C     HFLD          WHENEQ    'SCUWTR'
     C*
     C                   CALL      'S0005R'                             99
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      3             ##VIEW            1 0
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PUP03                              99
     C*
     C                   MOVEL(P)  UPSTAF        SCUWTR
     C                   MOVEL(P)  UPUSNM        SCUNAM
     C                   MOVE      *ON           *IN23
     C                   ELSE
     C                   MOVE      *ON           *IN24
     C                   ENDIF                                                  ##STAF<>*BLANKS
     C*
     C*_______________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCDRT#'
     C*
     C                   MOVE      *OFF          *IN90
     C*
     C*  CHECK FOR VALID GROUP NUMBER
     C*
     C     FNKY          CHAIN     WMFNL                              60
     C*
     C     *IN60         IFEQ      *ON
     C     FNSTAT        OREQ      'I'
     C                   SETON                                        71  90
     C                   ENDIF                                                  *IN60=*ON/FNSTAT=I
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   CALL      'W0640R'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      DBRRNF        ##RRN2            9 0
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     WMARP                              60
     C*
     C                   Z-ADD     ARRT#         SCDRT#
     C                   MOVE      *ON           *IN26
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN25
     C*
     C                   ENDIF                                                  ##RT#<>*ZEROS
     C*
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C*  IF F10 ATTEMPTED ON NON-SEARCHABLE FIELDS
     C*
     C     HFLD          WHENEQ    'SCAGT#'
     C                   MOVE      *ON           *IN91
     C                   MOVE      *ON           *IN22
     C*
     C     HFLD          WHENEQ    'SCEFDT'
     C                   MOVE      *ON           *IN92
     C                   MOVE      *ON           *IN28
     C*
     C     HFLD          WHENEQ    'SCCNDT'
     C                   MOVE      *ON           *IN94
     C                   MOVE      *ON           *IN29
     C*
     C     HFLD          WHENEQ    'SCEXDT'
     C                   MOVE      *ON           *IN95
     C                   MOVE      *ON           *IN26
     C*
     C     HFLD          WHENEQ    'SCCMMT'
     C                   MOVE      *ON           *IN96
     C                   MOVE      *ON           *IN27
     C*
     C     HFLD          WHENEQ    'SCCMDT'
     C                   MOVE      *ON           *IN97
     C                   MOVE      *ON           *IN32
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14
     C*===============================================================
     C*
     C*  F14=DARTS
     C*
     C     KNDO          BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   CLEAR                   QCKEY
     C*
     C                   Z-ADD     SCFEIN        QCFEIN
     C                   Z-ADD     SCMOD         QCMOD
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0040R'      XXPGM
     C*
     C                   ENDSR                                                  KNDO
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *ON
     C                   MOVE      'A'           AASTAT
     C                   MOVE      'Y'           AACMFL
     C                   MOVE      'Y'           AAMLFL
     C                   MOVE      'N'           AAPRFL
     C                   ENDIF                                                  *IN40=*ON
     C*
     C     @@MODE        IFEQ      'P'
     C                   Z-ADD     *ZERO         SCFND
      *
     C                   ELSE
      *
     C                   Z-ADD     AAFND         SCFND
     C                   ENDIF                                                  @@MODE NE 'P'
     C*
     C                   CLEAR                   SCFNAM
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNSHNM        SCFNAM
     C*
     C***  Begin Add  ***  03/25/19  ***********************************
      /Free
       scPortDft = AAportDft;
      /End-free
     C***  End   Add  ***  03/25/19  ***********************************
     C*
     C                   Z-ADD     AAAGT#        SCAGT#
     C                   MOVEL     AASTAT        SCSTAT
     C                   MOVEL     AACMFL        SCCMFL
     C                   MOVEL     AAMLFL        SCMLFL
     C                   MOVEL     AAPRFL        SCPRFL
     C                   Z-ADD     AACHAP        SCCHAP
     C                   Z-ADD     AACMMT        SCCMMT
     C*
     C                   Z-ADD     AACMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCMDT
     C*
     C                   MOVEL(P)  AAUWTR        SCUWTR
     C                   CLEAR                   SCUNAM
     C     AAUWTR        CHAIN     PUP02                              60
     C  N60              MOVEL(P)  UPUSNM        SCUNAM
     C*
     C                   Z-ADD     AADRT#        SCDRT#
     C*
     C                   Z-ADD     AAEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
     C*
     C                   Z-ADD     AAEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   Z-ADD     SCEFDT        SVEFDT
     C*
     C                   Z-ADD     AACNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCNDT
     C                   Z-ADD     SCCNDT        SVCNDT
     C*
     C                   CLEAR                   SCCHPD
     C     CPKY          CHAIN     WMCPL                              60
     C  N60              MOVEL(P)  CPDESC        SCCHPD
     C*
     C*  SAVE ORIGINAL AGT#, IF IT CHANGES, NEED TO UPADATE ALL QUOTE
     C*  RECORDS FOR THE ORIGINAL AGT#
     C*
     C                   Z-ADD     SCAGT#        SVAGT#
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         AACO#
     C                   Z-ADD     SCFND         AAFND
     C***  Begin Add  ***  03/25/19  ***********************************
      /Free
       AAportDft = scPortDft;
      /End-free
     C***  End   Add  ***  03/25/19  ***********************************
     C                   Z-ADD     SCFEIN        AAFEIN
     C                   Z-ADD     SCMOD         AAMOD
     C                   Z-ADD     SCAGT#        AAAGT#
     C                   MOVEL     SCSTAT        AASTAT
     C                   MOVEL     SCCMFL        AACMFL
     C                   MOVEL     SCMLFL        AAMLFL
     C                   MOVEL     SCPRFL        AAPRFL
     C                   Z-ADD     SCCHAP        AACHAP
     C                   Z-ADD     SCCMMT        AACMMT
     C*
     C                   Z-ADD     SCCMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AACMDT
     C*
     C                   MOVEL(P)  SCUWTR        AAUWTR
     C                   Z-ADD     SCDRT#        AADRT#
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AAEXDT
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AAEFDT
     C*
     C                   Z-ADD     SCCNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AACNDT
     C*
     C                   MOVEL(P)  SCNAME        AANAME
     C                   MOVEL(P)  SCFDNM        AAFDNM
     C                   MOVEL(P)  Q1USER        AAUSER
     C                   MOVEL(P)  Q1PGM         AAPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AADT
     C*
     C                   Z-ADD     DSTIME        AATIME
     C*
     C                   ENDSR                                                  MVSCDB
     C*
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR                                                  OTHERR
     C*
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKI         CASEQ     *ON           KIDO                     90
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKN         CASEQ     *ON           KNDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
      *
     C     @@MODE        IFEQ      'P'                                           COPY MODE
     C                   SETON                                        40
     C                   ENDIF
      *
     C                   MOVE      *OFF          *IN31
     C*
     C     @@MODE        IFEQ      'A'
     C     Q1PRMS        ANDGE     6
     C     @@PGM         ANDEQ     'W0040R02'
     C                   MOVE      *ON           *IN31
     C                   ENDIF                                                  @@MODE=A
     C*
     C     @@RRN2        CHAIN     WMAGP                              60
     C*
     C                   Z-ADD     AGFEIN        SCFEIN
     C                   Z-ADD     AGMOD         SCMOD
     C                   MOVEL(P)  AGNAME        SCNAME
     C                   MOVEL(P)  AGFDNM        SCFDNM
     C                   Z-ADD     AGCO#         ##CO#
     C*
     C     ##CO#         CHAIN     PCO01                              60
     C*
B02  C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        SCCONM
     C*
     C                   ENDIF                                                  *IN60=*OFF
      /Free
       If ##xfnd = 'Y';
          savFnd = ##FND;
          count = 0;
          Setll *Start WMfnp;
          Read WMfnp;
          Dow Not %Eof(WMfnp);
             ##RCDS = *On;
             ##APPL = 'W';
             ##AUTH = *Blanks;
             fndRest(##Co# : Q1user : ##APPL : ##RCDS : ##AUTH : FNfnd);
             If ##RCDS = *Off;
                count = count + 1;
                fndary(count) = FNfnd;
             Endif;
             Read WMfnp;
          Enddo;
          If count = 0;
             count = 1;
             fndary(count) = savFnd;
          Endif;
       Endif;
       //***  Begin Add  ***  03/25/19  ***********************************
       isPortal = IsModEnabled('Portal Module');
       strPortalName = GetModuleProperty('Portal Module':'Name');
       scPortDftH = 'Default Group for ' + strPortalName;
       //***  End   Add  ***  03/25/19  ***********************************
      /End-Free
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
     C*
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  PREOTH
     C*
     C*================================================================
     C* UPDTQT - Update quote status recods with new agent #
     C*================================================================
     C*
     C     UPDTQT        BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
     C     QTKY          CHAIN     PQT01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     SCAGT#        QTAGT#
     C                   MOVEL(P)  Q1USER        QTUSER
     C                   MOVEL(P)  Q1PGM         QTPGM
     C                   ADD       1             DSTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QTDT
     C*
     C                   Z-ADD     DSTIME        QTTIME
     C*
     C                   UPDATE    PQT01
     C*
     C     QTKY          READE     PQT01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  UPDTQT
     C*
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C*
     C     UPDATR        BEGSR
     C*
     C*  IF AGT# HAS BEEN CHANGED, UPDATE QUOTE RECORDS
     C*
     C     SCAGT#        IFNE      SVAGT#
     C                   EXSR      UPDTQT
     C                   ENDIF                                                  SVAGT#<>SCAGT#
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF                                                  *IN99=*OFF
     C*
     C*  IF WE HAD AN INACTIVE DUPLICATE, DELETE IT NOW
     C*
     C     SVFEIN        IFNE      *ZERO
     C     AAKY1         CHAIN     PAA01                              60
     C  N60DBRRNA        DELETE    WMAAP                              60
     C                   ENDIF                                                  SVFEIN<>*ZERO
     C*
     C                   ENDSR                                                  UPDATR
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
     C                   MOVEL(P)  Q1USER        AAIUSR
     C                   MOVEL(P)  Q1PGM         AAIPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AAIDT
     C*
     C                   Z-ADD     DSTIME        AAITM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMAAP
     C*
     C*  IF WE HAD AN INACTIVE DUPLICATE, DELETE IT NOW
     C*
     C     SVFEIN        IFNE      *ZERO
     C     AAKY1         CHAIN     PAA01                              60
     C  N60DBRRNA        DELETE    WMAAP                              60
     C                   ENDIF                                                  SVFEIN<>*ZERO
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  WRITER
     C*
