     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0042R02 - AGENT PRODUCER INFORMATION
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE AGENT PRODUCER FILE.     *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0042R - WORK WITH AGENT PRODUCERS                         *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    SC010R - DARTS                                             *
     H*                                                               *
     H*  CREATED: 12/01/97  PROGRAMMER: C:33  PROJECT: WC683          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 04/20/21 C:298 Project: WC2278                                *
     H*                Multiple selection entries are be added for:   *
     H*                Distribution Types, Sub Categories and         *
     H*                Process Output Control                         *
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     h*  07/02/15 C:306 Support: SEQ86962 / 31805                    *
     h*                 Added the email validation check             *
     h*
     H*  04/26/07 C:183 Support: WA081/58113                          *
     H*                 Changed 'NPN Number' to 'NPN' in the DSPF.    *
     H*                                                               *
     H*  12/15/06 C:183 PROJECT 81WYT                                 *
     H*                 Added Agent NPN Number Logic.                 *
     H*                                                               *
     H*  03/06/02 C:132 PROJECT WC1071                                *
     H*                 Add Cancelllation Date field                  *
     H*                                                               *
     H*  10/08/01 C:136 PROJECT WC577                                 *
     H*                 Add E-mail address field.                     *
     H*                                                               *
     H*  01/25/01 C:33  SEQ#: WA081/25482                             *
     H*                 REMOVE EDIT WORD FROM SCSS#.                  *
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H*  12/02/99  C:33  Y2K#: 21                                     *
     H*                  LOAD MAINTENANCE FIELDS TO FILE              *
     H*                                                               *
     H*  06/24/98  C:33  SEQ#: WA095/12231                            *
     H*                  LOAD THE CONFIRMED DATE TO FILE              *
     H*                                                               *
     H*  06/19/98  C:33  SUPPORT: CONF                                *
     H*                  IN ADD MODE BE SURE THAT THE PRODUCER NAME   *
     H*                  IS DISPLAYED ON SCR02                        *
     H*****************************************************************
     FW0042F02  CF   E             WORKSTN
     F                                     Handler('PROFOUNDUI(HANDLER)')
     F*
     F*  Physical file definition (for maintaining record)
     FWMAPP     UF A E             DISK
     F                                     RENAME(PAP01:DBRCDF)
     FWMAGP     IF   E             DISK
     FSMCOL     IF   E           K DISK
     FWMAPL     IF   E           K DISK
     FWMPSL     IF   E           K DISK
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
      ***** Beg Add ***** 04/20/21 *********************************
     fwmappcrt  uf a e           k disk    rename(pap01:pap0p)
     f                                     prefix(CR_)
      ***** End Add ***** 04/20/21 *********************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d charSS#         s              9
      ***  End   Add  ***  06/09/20  ****************************
      ***** Beg Add ***** 04/20/21 *********************************
     d/copy sPrString
      ***** End Add ***** 04/20/21 *********************************
     D***  BEGIN ADD  ***  02/04/98  *********************************
     D                 DS
     D  QCKEY                  1     40
     D  QCFEIN                 1      9  0
     D  QCMOD                 10     12  0
     D***  END   ADD  ***  02/04/98  *********************************
     D*
      ***** Beg Add ***** 04/20/21 *********************************
     d strDisval       s             25
     d strSubVal       s             25
     d strOCoVal       s             25
     d maxseq#         s             15  0
      ***** End Add ***** 04/20/21 *********************************
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Agent Producers')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(2)

     C*
      ****** beg add ********* 07/02/15 ****************************************
     d/copy sPrGenErr
      ****** end add ********* 07/02/15 ****************************************
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
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
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
     C*  AGENT PRODUCER FILE
     C*
     C     APKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C                   KFLD                    SCPROD
     C*
     C*  PRODUCER NON-RESIDENT FILE
     C*
     C     PSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C                   KFLD                    SCPROD
     C                   KFLD                    SCRST
     C*
     C*  SYSTEM TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C***  BEGIN ADD  ***  02/04/98  *********************************
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C***  END   ADD  ***  02/04/98  *********************************
     C     *LIKE         DEFINE    STKEY         XXKEY
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
     C     APSTAT        IFEQ      'I'
     C                   MOVE      'A'           APSTAT
     C                   ELSE
     C                   MOVE      'I'           APSTAT
     C                   ENDIF                                                  APSTAT=I
     C*
     C                   MOVEL(P)  Q1USER        APUSER
     C                   MOVEL(P)  Q1PGM         APPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         APDT
     C*
     C                   MOVEL     DSTIME        APTIME
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
     C  N90*IN40         IFEQ      *ON
     C*
     C*  PRODUCER NUMBER MUST BE ENTERED
     C*
     C  N90SCPROD        IFLE      *ZEROS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCPROD<=*ZEROS
     C*
     C*  BE SURE THE PROD # HAS NOT BEEN PREVIOUSLY BEEN USED
     C*
     C  N90APKY          CHAIN     PAP01                              60
     C  N90
     CANN60              SETON                                        72  90
     C*
     C                   ENDIF                                                  *IN40=*ON
     C*
     C*  LAST NAME MAY NOT BE BLANK
     C*
     C  N90SCLSN         IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCLSN=*BLANKS
     C*
     C*  IF ADDRESS IS ENTERED, ALL OTHER FIELDS MUST BE ENTERED
     C*
     C  N90SCMAD1        IFNE      *BLANKS
     C*
     C  N90SCMCTY        IFEQ      *BLANKS
     C                   SETON                                        74  90
     C                   ENDIF                                                  SCMCTY=*BLANKS
     C*
     C                   MOVEL(P)  SCMST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        75  90
     C*
     C  N90SCMZP1        IFEQ      *ZEROS
     C                   SETON                                        76  90
     C                   ENDIF                                                  SCMZP1<>*BLANKS
     C*
     C                   ENDIF                                                  SCMAD1<>*BLANKS
     C*
     C*  VALIDATE DATE OF BIRTH
     C*
     C     SCDOB         IFNE      *ZEROS
     C*
     C                   Z-ADD     SCDOB         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDOB
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        77  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCDOB<>*ZEROS
     C*
     C***** BEGIN ADD ***** 03/06/02 ***********************************
     C*  VALIDATE Canelllation Date
     C*
     C     SCCXDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCXDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        82  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCCXDT<>*ZEROS
     C***** END ADD ******* 03/06/02 ***********************************
     C*
     C*  LICENSE FLAG MUST BE 'Y' OR BLANK
     C*
     C  N90SCLFLG        IFNE      *BLANK
     C     SCLFLG        ANDNE     'Y'
     C                   SETON                                        78  90
     C                   ENDIF                                                  SCLFLG<>*BLANK/Y
     C*
     C*  LICENSE STATE MUST BE VALID
     C*
     C                   MOVEL(P)  SCRST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        79  90
     C*
     C*  RESIDENT STATE CANNOT BE IN NON-RESIDENT STATE FILE
     C*
     C  N90PSKY          CHAIN     WMPSL                              60
     C  N90
     CANN60              SETON                                        89  90
     C*
     C*  VALIDATE LICENSE RENEWAL DATE
     C*
     C  N90SCRWDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCRWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRWDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        80  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCRWDT<>*ZEROS
     C*
     C*  VALIDATE LICENSE EXPIRATION DATE
     C*
     C  N90SCEXDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        81  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCEXDT<>*ZEROS
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90APDT          IFGT      SAVEDT
     C     APDT          OREQ      SAVEDT
     C     APTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  APDT>SAVEDT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C***  BEGIN ADD  ***  06/19/98  *********************************
     C     SCLSN         CAT(P)    ',':0         SCPNAM
     C     SCPNAM        CAT       SCFSN:1       SCPNAM
     C***  BEGIN ADD  ***  06/19/98  *********************************
     ***** Begin Chg ***** 07/02/15 **********************************
      /Free
           erscemal = *Off;
            If %Len(%trim(scemal)) > 0;
               erscemal = Check_Email(scemal:msgscemal);
              If erscemal = *On;
                 *IN90 = *On;
              EndIf;
            EndIf;
      /End-free
     ***** End Chg ***** 07/02/15 **********************************
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C*  VALIDATE DATE FILED WITH CARRIER
     C*
     C  N90SCFIDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCFIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFIDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        71  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCFIDT<>*ZEROS
     C*
     C*  VALIDATE DATE MAILED TO STATE
     C*
     C  N90SCMLDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCMLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCMLDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        72  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCMLDT<>*ZEROS
     C*
     C*  VALIDATE CONFIRMED DATE
     C*
     C  N90SCCFDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCCFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCFDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        73  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCCFDT<>*ZEROS
     C*
     C*  VALIDATE DENIED DATE
     C*
     C  N90SCDNDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCDNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDNDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        74  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCDNDT<>*ZEROS
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90APDT          IFGT      SAVEDT
     C     APDT          OREQ      SAVEDT
     C     APTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  APDT>SAVEDT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
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
     C* KJDO - Processing for F10
     C*================================================================
     C*
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCCHAP'
     C** *****
     C** *****             MOVE *OFF      *IN90
     C** *****
     C** CHECK FOR VALID GROUP NUMBER
     C** *****
     C** *****   FNKY      CHAINWMFNL                60
     C** *****
     C** *****   *IN60     IFEQ *ON
     C** *****   FNSTAT    OREQ 'I'
     C** *****             SETON                     71  90
     C** *****             ENDIF                           *IN60=*ON/FNSTAT=I
     C** *****
     C** *****   *IN90     IFEQ *OFF
     C** *****
     C** *****             CALL 'W0720R'               99
     C** *****   *INKC     PARM           ##KC    1
     C** *****   *INKL     PARM           ##KL    1
     C** *****             PARM '2'       ##CALL  1
     C** *****             PARM           ##RRN   90
     C** *****             PARM DBRRNF    ##RRN2  90
     C** *****             PARM *ZEROS    ##CHAP
     C** *****
     C** *****   ##CHAP    IFNE *ZEROS
     C** *****
     C** *****             Z-ADD##CHAP    SCCHAP
     C** *****   CPKY      CHAINPCP01                60
     C** *****             MOVE *ON       *IN24
     C** *****
     C** *****             ELSE
     C** *****
     C** *****             MOVE *ON       *IN23
     C** *****
     C** *****             ENDIF                           ##CHAP<>*ZEROS
     C** *****
     C** *****             MOVE *ON       *IN90
     C** *****
     C** *****             ENDIF                           *IN90=*OFF
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
     C*
     C***  BEGIN ADD  ***  02/04/98  *********************************
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
     C***  END   ADD  ***  02/04/98  *********************************
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C   40              MOVE      'A'           APSTAT
     C*
     C                   MOVE      APSTAT        SCSTAT
     C                   Z-ADD     APPROD        SCPROD
     C                   MOVEL(P)  APLSN         SCLSN
     C                   MOVEL(P)  APFSN         SCFSN
     C*
     C     SCLSN         CAT(P)    ',':0         SCPNAM
     C     SCPNAM        CAT       SCFSN:1       SCPNAM
     C*
     C                   MOVEL(P)  APCOMM        SCCOM1
     C                   MOVEL(P)  APCOM2        SCCOM2
     C** *****             Z-ADDAPCHAP    SCCHAP
     C*
     C                   Z-ADD     APDOB         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDOB
     C*
     C***** BEGIN ADD ***** 03/06/02 ***********************************
     C                   Z-ADD     APCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C***** END ADD ******* 03/06/02 ***********************************
     C*
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              MOVEL(P)  APSS#         SCSS#
      /Free
       charSS# = %EditC(APSS#:'X');
       SCSS# = MaskData('SSN':'APSS#':charSS#);
      /End-Free
      *****  End Chg  ***  06/09/20  **********************
     C                   MOVEL(P)  APMAD1        SCMAD1
     C                   MOVEL(P)  APMCTY        SCMCTY
     C                   MOVEL(P)  APMST         SCMST
     C                   MOVEL     APMZIP        SCMZP1
     C                   MOVE      APMZIP        SCMZP2
      *** BEG ADD **** 10/08/01 ************************************************
     C                   MOVEL(P)  APEMAL        SCEMAL
      *** END ADD **** 10/08/01 ************************************************
     C*
     C                   Z-ADD     APFIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFIDT
     C*
     C                   Z-ADD     APMLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMLDT
     C*
     C                   Z-ADD     APCFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCFDT
     C*
     C                   Z-ADD     APDNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDNDT
     C***  BEGIN ADD  ***  12/15/06  *********************************
     C                   Z-ADD     APNPN         SCNPN
     C***  END   ADD  ***  12/15/06  *********************************
     C*
     C                   MOVEL     APLFLG        SCLFLG
     C                   MOVEL     APRST         SCRST
     C                   MOVEL(P)  APLIC#        SCLIC#
     C                   MOVEL(P)  APLTYP        SCLTYP
     C                   MOVEL(P)  APLTP2        SCLTP2
     C                   MOVEL(P)  APLTP3        SCLTP3
     C                   MOVEL(P)  APLTP4        SCLTP4
     C                   MOVEL(P)  APLTP5        SCLTP5
     C*
     C                   Z-ADD     APEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
     C*
     C                   Z-ADD     APRWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRWDT
     C*
      /Free
       scDisTyp = ' ';
       scSubCate = ' ';
       scOutCont = ' ';
       Setll (##co#:scFein:scMod:scProd:APseq#) pap0p;
       Reade (##co#:scFein:scMod:scProd:APseq#) pap0p;
       Dow not %eof(wmappcrt);
       If CR_APcritid = 'Distribution';
          If scDisTyp = ' ';
             scDisTyp = CR_APcritval;
          Else;
             scDisTyp = %trim(scDisTyp) + ',' + %trim(CR_APcritval);
          Endif;
       Endif;
       If CR_APcritid = 'Sub Category';
          If scSubCate = ' ';
             scSubCate = CR_APcritval;
          Else;
             scSubCate = %trim(scSubCate) + ',' + %trim(CR_APcritval);
          Endif;
       Endif;
       If CR_APcritid = 'Process Output Control';
          If scOutCont = ' ';
             scOutCont = CR_APcritval;
          Else;
             scOutCont = %trim(scOutCont) + ',' + %trim(CR_APcritval);
          Endif;
       Endif;
       Reade (##co#:scFein:scMod:scProd:APseq#) pap0p;
       Enddo;
      /End-free
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         APCO#
     C                   Z-ADD     SCFEIN        APFEIN
     C                   Z-ADD     SCMOD         APMOD
     C                   MOVE      SCSTAT        APSTAT
     C                   Z-ADD     SCPROD        APPROD
     C                   MOVEL(P)  SCLSN         APLSN
     C                   MOVEL(P)  SCFSN         APFSN
     C                   MOVEL(P)  SCCOM1        APCOMM
     C                   MOVEL(P)  SCCOM2        APCOM2
     C** *****             Z-ADDSCCHAP    APCHAP
     C*
     C                   Z-ADD     SCDOB         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         APDOB
     C*
     C***** BEGIN ADD ***** 03/06/02 ***********************************
     C                   Z-ADD     SCCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         APCXDT
     C***** END ADD ******* 03/06/02 ***********************************
     C*
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              MOVEL(P)  SCSS#         APSS#
      /Free
       clear charSS#;
       charSS# = UnMaskData('SSN':'APSS#':SCSS#);
       APSS# = %dec(charSS#:9:0);
      /End-Free
      *****  End Chg  ***  06/09/20  **********************
     C                   MOVEL(P)  SCMAD1        APMAD1
     C                   MOVEL(P)  SCMCTY        APMCTY
     C                   MOVEL(P)  SCMST         APMST
     C                   MOVEL     SCMZP1        APMZIP
     C                   MOVE      SCMZP2        APMZIP
      *** BEG ADD **** 10/08/01 ************************************************
     C                   MOVEL(P)  SCEMAL        APEMAL
      *** END ADD **** 10/08/01 ************************************************
     C*
     C                   Z-ADD     SCFIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         APFIDT
     C*
     C                   Z-ADD     SCMLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         APMLDT
     C*
     C                   Z-ADD     SCCFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C***  BEGIN CHG  ***  06/24/98  *********************************
     C**********           MOVELSRDTO     SCCFDT
     C                   MOVEL     SRDTO         APCFDT
     C***  END   CHG  ***  06/24/98  *********************************
     C*
     C                   Z-ADD     SCDNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         APDNDT
     C***  BEGIN ADD  ***  12/15/06  *********************************
     C                   Z-ADD     SCNPN         APNPN
     C***  END   ADD  ***  12/15/06  *********************************
     C*
     C                   MOVEL     SCLFLG        APLFLG
     C                   MOVEL     SCRST         APRST
     C                   MOVEL(P)  SCLIC#        APLIC#
     C                   MOVEL(P)  SCLTYP        APLTYP
     C                   MOVEL(P)  SCLTP2        APLTP2
     C                   MOVEL(P)  SCLTP3        APLTP3
     C                   MOVEL(P)  SCLTP4        APLTP4
     C                   MOVEL(P)  SCLTP5        APLTP5
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         APEXDT
     C*
     C                   Z-ADD     SCRWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         APRWDT
     C*
     C***  BEGIN ADD  ***  12/02/99  *********************************
     C                   MOVEL(P)  Q1USER        APUSER
     C                   MOVEL(P)  Q1PGM         APPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         APDT
     C*
     C                   MOVEL     DSTIME        APTIME
     C***  END   ADD  ***  12/02/99  *********************************
     C*
      ***** Beg Add ***** 04/20/21 ***********************************
      /Free
       If @@mode = 'C';
          Setll (##co#:scFein:scMod:scProd:APseq#) pap0p;
          Reade (##co#:scFein:scMod:scProd:APseq#) pap0p;
          Dow not %eof(wmappcrt);
             Delete wmappcrt;
             Reade (##co#:scFein:scMod:scProd:APseq#) pap0p;
          Enddo;
          Unlock wmappcrt;
       Endif;

       If scDisTyp <> ' ';
          Dow scDisTyp <> ' ';
          Clear strDisval;
             strDisval = ParseString(scDisTyp:',':1:'1');
             CR_APcritid = 'Distribution';
             CR_APcritval = strDisVal;
             Exsr Write_CritRec;
          Enddo;
       Endif;
       If scSubCate <> ' ';
          Dow scSubCate <> ' ';
          Clear strSubVal;
             strSubVal = ParseString(scSubCate:',':1:'1');
             CR_APcritid = 'Sub Category';
             CR_APcritval = strSubVal;
             Exsr Write_CritRec;
          Enddo;
       Endif;
       If scOutCont <> ' ';
          Dow scOutCont <>  ' ';
          Clear strOCoVal;
             strOCoVal = ParseString(scOutCont:',':1:'1');
             CR_APcritid = 'Process Output Control';
             CR_APcritval = strOCoVal;
             Exsr Write_CritRec;
          Enddo;
       Endif;

      /End-free
      ***** End Add ***** 04/20/21 ***********************************
     C                   ENDSR                                                  MVSCDB
      ***** Beg Add ***** 04/20/21 ****************************************
      /Free
       Begsr Write_CritRec;
          CR_APseq# = APseq#;
          CR_APco#  = ##co#;
          CR_APfein = scFein;
          CR_APmod  = scMod;
          CR_APProd = scProd;
          CR_APstat = APstat;
          CR_APiusr = q1user;
          CR_APipgm = q1pgm;
          CR_APuser = q1user;
          CR_APpgm  = q1pgm;
      /End-free
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CR_APidt
     C                   MOVEL     SRDTO         CR_APdt
     C                   MOVEL     DSTIME        CR_APitm
     C                   MOVEL     DSTIME        CR_APtime
      /Free
          Write pap0p;
          Unlock Wmappcrt;
       Endsr;
      /End-free
      ***** End Add ***** 04/20/21 ****************************************
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
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C***  BEGIN ADD  ***  02/04/98  *********************************
     C     *INKN         CASEQ     *ON           KNDO                     90
     C***  END   ADD  ***  02/04/98  *********************************
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
     C*
     C     @@RRN2        CHAIN     WMAGP                              60
     C*
     C                   Z-ADD     AGFEIN        SCFEIN
     C                   Z-ADD     AGMOD         SCMOD
     C                   MOVEL(P)  AGNAME        SCANAM
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
     C*
      /End-free
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
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF                                                  *IN99=*OFF
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
      ***** Beg Add ***** 04/20/21 **************************************
      /Free
       Exec sql SET OPTION commit=*none;
       Exec SQL Select Max(APseq#) Into :maxseq#  From Wmapl
       Where APco# = :##co# And APfein = :scfein And APmod = :scmod;
       APseq# = maxseq# + 1;
      /End-free
      ***** End Add ***** 04/20/21 **************************************
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL(P)  Q1USER        APIUSR
     C                   MOVEL(P)  Q1PGM         APIPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         APIDT
     C*
     C                   Z-ADD     DSTIME        APITM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMAPP
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  WRITER
