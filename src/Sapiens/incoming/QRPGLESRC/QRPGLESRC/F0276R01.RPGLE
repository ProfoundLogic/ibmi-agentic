     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H**********************************************************
     H* PROGRAM: F0276R01
     H* DESCRIPTION: MEDICAL SERVICES
     H**********************************************************
     H* INDICATORS USEAGE:
     H*
     H* GENERAL
     H*  99 - GENERAL CHAIN
     H*
     H* SCR01 (KJDO)
     H*  30 - POSITION CURSOR TO SCBGDT.
     H*  31 - POSITION CURSOR TO SCSTS.
     H*  32 - POSITION CURSOR TO SCCLCD.
     H*  33 - POSITION CURSOR TO SCICD9.
     H*  34 - POSITION CURSOR TO SCDPR#.
     H*
     H* SCR01 (ERRCK1)
     H*  71 - SERVICE CODE CANNOT BE BLANK
     H*  72 - INVALID SERVICE CODE
     H*  73 - BEGIN DATE CANNOT BE ZERO
     H*  74 - INVALID BEGIN DATE
     H*  75 - INVALID END DATE
     H*  76 - END DATE CANNOT BE ZERO
     H*  77 - INVALID CLOSURE STATUS
     H*  78 - CLOSURE STATUS CANNOT BE BLANK
     H*  79 - STAFF CODE CANNOT BE BLANK
     H*  80 - INVALID STAFF CODE
     H*  81 - INVALID DIANOSIS/TYPE CODE
     H*  82 - INACTIVE DIAGNOSIS/TYPE CODE
     H*  83 - END DATE CANNOT BE LESS THAN BEGIN DATE
     H*
     H****************************************************************
     H* REVISIONS:                                                   *
     H*                                                              *
     H* 08/05/13 C:277 PROJECT: MC329 Phase II                       *
     H*                Add search functionality and Validate ICD10   *
     H*                                                              *
     H* 05/23/13 C:277 PROJECT: MC329                                *
     H*                 Activate the ICD10 fields if ICD10 is active *
     H*                                                              *
     H* 07/01/99 C:62 PROJECT: MC207                                 *
     H*               SWAP OUT DIAGNOSIS CODE SEARCH WITH F2012R.    *
     H*                                                              *
     H* 01/28/98 C:62 PROJECT: MC145                                 *
     H*               1) HEADING CHANGED TO 'CASE MANAGEMENT SERVICE-*
     H*                  INFORMATION'                                *
     H*               2) FIELD HEADING CHANGED TO 'CASE MGMT SERVICE-*
     H*                  CODE'                                       *
     H*               3) ADDED NEW SEARCHES                          *
     H*                        F0510R TO F2010R - ICD9 CODES         *
     H*                        F2201R TO F2410R - COVERAGE CODES     *
     H*                        F2206R TO F2415R - CLOSURE CODES      *
     H*                        S0507R TO S0005R - STAFF CODES        *
     H*               4) UPPER/LOWER CASE ENTRY ALLOWED.             *
     H*               5) DATES CONVERTED BEFORE SEARCHES.            *
     H*               6) MAINTENANCE FIELDS ADDED FOR ADD.           *
     H*               7) SEQ# INCREMENTS AND ZERO SUPPRESSED.        *
     H*               8) RECORD IS NOT ADDED IN ERROR.               *
     H*               9) IN DISPLAY MODE NOW CAN ENTER OUT.          *
     H*                                                              *
     H* 01/08/98 C:86 PROJECT: MC145                                 *
     H*          1.   DISPLAY FILE F0276F01 HAS BEEN CHANGED TO      *
     H*               TO PRINT A 2 LINE SUB-HEADING. THIS PRO-       *
     H*               GRAM CALLS FA702R TO PRINT IT & USES LIKE      *
     H*               DEFINES TO REDEFINE THE OMMITTED SCREEN        *
     H*               FIELDS.                                        *
     H*          2.   DARTS CALL WITH OPTION 14 TO SC010R HAS        *
     H*               BEEN REPLACED WITH A CALL TO FA703R AND A      *
     H*               CALL TO SA001R FOR CORRECT FLAGS & INFO        *
     H*                                                              *
     H* 11/06/97 C:53  MC145                                         *
     H*                CHG KEY DARTS PGM FROM F0075R TO F0270R       *
     H*                                                              *
     H* 06/24/97 C:56  PROJECT: 87MAZ                                *
     H*                ADD WRITE AND UPDATE OF MSSTAT                *
     H*                                                              *
     H* 06/23/97 C:56  PROJECT: 87MAZ                                *
     H*                APPLY NEW DISPLAY SCREEN STANDARDS            *
     H*                                                              *
     H* 02/18/97 C:56  87MAZ                                         *
     H*                CREATED                                       *
     H*                                                              *
     H****************************************************************
     FF0276F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FFAMSP     UF A E             DISK
     F                                     RENAME(PMS01:DBRCDF)
     FFAMSL3    IF   E           K DISK
     FFAMSL5    IF   E           K DISK
     F                                     RENAME(PMS01:PMS02)
     FFTMCL     IF   E           K DISK
     FFTMCP     IF   E             DISK
     F                                     RENAME(PMC01:PMC02)
     FFTCLL     IF   E           K DISK
     FFTCLP     IF   E             DISK
     F                                     RENAME(PCL01:PCL02)
     FSMUPL1    IF   E           K DISK
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP02)
     FFMDIL     IF   E           K DISK
     FFMCMP     IF   E             DISK
     FFADAP     IF   E             DISK
     F*** Beg Add *** 08/05/13 ***************************************
     Ffmicd10l1 if   e           k disk
     F*** End Add *** 08/05/13 ***************************************
      /COPY SCOPYMT01D
     D*
     D OPTNM           C                   CONST('Case Management Ser-
     D                                     vices Information')
     D COMAPI          C                   CONST('FA007R')
     D*
     D*** DEFAULT DATE AND TIME
     D*
     D                 DS
     D  DFDTTM                 1     12  0
     D  DFTIME                 1      6  0
     D  DFDATE                 7     12  0
     D***Beg Add *** 05/23/13 ***************************************
     D SA015R          pr                  extpgm('SA015R')
     D @@rtcd                         1
     D*
     D xxicd10         s              1
     D @@rtcd          s              1
     D***End Add *** 05/23/13 ***************************************
     D*** Beg Add *** 08/05/13 ***************************************
     D kkicd10         s                   like(iccode)
     D*** End Add *** 08/05/13 ***************************************

     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          RRN FAMSP
     C                   PARM                    @@RRN2            9 0          RRN FMCMP
     C*
     C     ACDPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C*** KEY LIST FOR FAMSP
     C*
     C     MSKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKSEQ#
     C*
     C     MSKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     MSKY3         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKMSCD
     C*
     C*** KEY LIST FOR MEDICAL SERVICE TABLE CODE FILE
     C*
     C     MCKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     C*
     C*** KEY LIST FOR CLOSURE CODE TABLE FILE
     C*
     C     CLKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCKEY
     C*
     C*** KEY LIST FOR USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    KKSTAF
     C*
     C*** KEY LIST FOR DIAGNOSIS CODE MASTER FILE
     C*
     C     DIKEY         KLIST
     C                   KFLD                    KKICD9
     C                   KFLD                    KKLOST
     C*
     C*** Beg Add *** 08/05/13 ***************************************
     C*** KEY LIST FOR DIAGNOSIS CODE MASTER FILE
     C     fmicd10l1key  klist
     C                   kfld                    kkicd10
     C                   kfld                    kklost
     C*** End Add *** 08/05/13 ***************************************
     C     *LIKE         DEFINE    MSBGDT        HLBGDT
     C     *LIKE         DEFINE    MSENDT        HLENDT
     C     *LIKE         DEFINE    MCKEY         KKKEY
     C     *LIKE         DEFINE    CLKEY         KKCKEY
     C     *LIKE         DEFINE    UPSTAF        KKSTAF
     C     *LIKE         DEFINE    DIICD9        KKICD9
     C     *LIKE         DEFINE    DILOST        KKLOST
     C     *LIKE         DEFINE    MSCASE        KKCASE
     C     *LIKE         DEFINE    MSFYR         KKFYR
     C     *LIKE         DEFINE    MSFND         KKFND
     C     *LIKE         DEFINE    MSSUB#        KKSUB#
     C     *LIKE         DEFINE    MSSEQ#        KKSEQ#
     C     *LIKE         DEFINE    MSMSCD        KKMSCD
     C*
     C                   Z-ADD     1             #OFSCR            1 0
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DATCVT - RUNS A DATCHK TO GO FROM 6 TO 8.
     C*================================================================
     C     DATCVT        BEGSR
     C*
     C                   Z-ADD     SCBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCBGDT
     C*
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCENDT
     C*
     C                   ENDSR
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
     C* SETOF POSITION TO CURSORS 30-34.
     C*
     C                   MOVEA     '00000'       *IN(30)
     C*
     C*** VALIDATE MEDICAL SERVICE CODE
     C*
     C     SCMSCD        IFEQ      *BLANKS
     C                   SETON                                        9071
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C*
     C     SCMSCD        IFNE      *BLANKS
     C                   MOVEL     SCMSCD        KKKEY
     C     MCKY          CHAIN     PMC01                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        9072
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C                   ENDIF
     C*
     C* CODE SHOULD BE USED ONLY ONCE
     C*
     C                   MOVE      CMSUB#        KKSUB#
     C                   MOVE      CMFND         KKFND
     C                   MOVE      CMFYR         KKFYR
     C                   MOVE      CMCASE        KKCASE
     C                   MOVEL     SCMSCD        KKMSCD
     C*
     C     MSKY3         CHAIN     PMS02                              99
     C*
     C     *IN90         DOWEQ     *OFF
     C     *IN99         ANDEQ     *OFF
     C     *IN40         ANDEQ     *ON
     C     *IN84         ANDEQ     *OFF
     C*
     C     MSSTAT        IFEQ      'O'
     C*
     C                   SETON                                        9084
     C*
     C                   ENDIF
     C*
     C     MSKY3         READE     PMS02                                  99
     C*
     C                   ENDDO
     C*
     C*** VALIDATE BEGIN DATE
     C*
     C     SCBGDT        IFEQ      *ZEROS
     C                   SETON                                        9073
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C*
     C     SCBGDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCBGDT
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9074
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCBGDT IFNE
     C*
     C*** VALIDATE ENDDATE IF ENTERED OR IF CLOSURE STATUS NOT BLANK
     C*
     C     SCENDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCENDT
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9075
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF                                                  SRERR IFEQ
     C                   ENDIF                                                  SCENDT IFEQ
     C*
     C     SCSTS         IFNE      *BLANKS
     C*
     C     SCENDT        IFEQ      *ZEROS
     C                   SETON                                        9076
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C*
     C     SCENDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCENDT
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9075
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF                                                  SRERR IFEQ
     C                   ENDIF                                                  SCENDT IFNE
     C                   ENDIF                                                  SCSTS IFNE
     C*
     C*** CHECK IF END DATE IS LESS THAN BEGIN DATE
     C*
     C     SCBGDT        IFNE      *ZEROS
     C     SCENDT        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLBGDT
     C*
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLENDT
     C*
     C     HLENDT        IFLT      HLBGDT
     C                   SETON                                        9083
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C                   ENDIF
     C*
     C*** VALIDATE CLOSURE STATUS IF ENTERED OR IF ENDDATE NOT ZERO
     C*
     C     SCSTS         IFNE      *BLANKS
     C                   MOVEL     SCSTS         KKCKEY
     C     CLKY          CHAIN     PCL01                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        9077
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF                                                  *IN99 IFEQ
     C                   ENDIF                                                  SCSTS IFNE
     C*
     C     SCENDT        IFNE      *ZEROS
     C*
     C     SCSTS         IFEQ      *BLANKS
     C                   SETON                                        9078
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C*
     C     SCSTS         IFNE      *BLANKS
     C                   MOVEL     SCSTS         KKCKEY
     C     CLKY          CHAIN     PCL01                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        9077
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF                                                  *IN99 IFEQ
     C                   ENDIF                                                  SCSTS IFNE
     C                   ENDIF                                                  SCENDT IFNE
     C*
     C*** VALIDATE STAFF CODE
     C*
     C     SCSTAF        IFEQ      *BLANKS
     C                   SETON                                        9079
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C*
     C     SCSTAF        IFNE      *BLANKS
     C                   MOVEL     SCSTAF        KKSTAF
     C     UPKEY         CHAIN     PUP01                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        9080
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C                   ENDIF
     C*
     C*** VALIDATE DIAGNOSIS CODE IF ENTERED
     C*
     C     SCICD9        IFNE      *BLANKS
     C     SCLOST        ORNE      *BLANKS
     C*
     C                   MOVEL     SCICD9        KKICD9
     C                   MOVEL     SCLOST        KKLOST
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkicd10 = scICd9;
          Chain fmicd10l1key pic01;
          If not %found(fmicd10l1);
             *in90 = *on;
             *in81= *on;
             leavesr;
          Else;
             If ICstat = 'I';
                *in90 = *on;
                *in82 = *on;
                leavesr;
             Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C     DIKEY         CHAIN     PDI01                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        9081
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C     DISTAT        IFEQ      'I'
     C                   SETON                                        9082
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF                                                  DISTAT IFEQ
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   ENDIF                                                  SCICD9 IFNE
     C*
     C     ERRSTP        ENDSR
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
     C* KJDO - F10=SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*
     C* SETOF POSITION TO CURSORS 30-34.
     C*
     C                   MOVEA     '00000'       *IN(30)
     C*
     C                   EXSR      DATCVT
     C*
     C*** MEDICAL SERVICE CODE SEARCH
     C*
     C     HFLD          IFEQ      'SCMSCD'
     C*
     C                   CALL      'F2425R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        90
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PMC02                              99
     C*
     C                   MOVEL     MCKEY         SCMSCD
     C                   MOVEL     MCDESC        SCDESC
     C*
     C                   SETON                                        9030
     C*
     C                   ENDIF                                                  *INKL EQ *ON
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C*** CLOSURE STATUS SEARCH
     C*
     C     HFLD          IFEQ      'SCSTS'
     C*
     C                   CALL      'F2415R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        9031
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PCL02                              99
     C*
     C                   MOVEL     CLKEY         SCSTS
     C                   MOVEL     CLDESC        SCSDSC
     C                   SETON                                        9032
     C*
     C                   ENDIF                                                  *INKL EQ *ON
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C*** STAFF CODE SEARCH
     C*
     C     HFLD          IFEQ      'SCSTAF'
     C*
     C                   CALL      'S0005R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      3             ##VIEW            1 0
     C*
     C     ##RRN         IFEQ      *ZEROS
     C                   SETON                                        9032
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PUP02                              99
     C*
     C                   MOVEL     UPSTAF        SCSTAF
     C                   MOVEL     UPUSNM        SCNAME
     C                   SETON                                        9033
     C*
     C                   ENDIF                                                  *INKL EQ *ON
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C*** DIAGNOSIS CODE AND TYPE SEARCH
     C*
     C     HFLD          IFEQ      'SCICD9'
     C*
     C*** Beg Chg *** 08/05/13 ****************************************
     C                   If        scICDind <> 'Y'
     C                   eval      xxicd10 = '0'
     C                   eval      ##LOST = *Blanks
     C                   else
     C                   eval      xxicd10 = '1'
     C                   eval      ##LOST = 'D'
     C                   Endif
     C*** End Chg *** 08/05/13 ****************************************
     C                   CALL      'F2012R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0          FADAP FILE
     C*** Beg Chg *** 08/05/13 ****************************************
     C********           PARM      *BLANKS       ##ICD9            5            ICD9 FILE
     C********           PARM      *BLANKS       ##LOST            1            ICD9 FILE
     C                   PARM      *BLANKS       ##ICD9            7            ICD9 FILE
     C                   PARM                    ##LOST            1            ICD9 FILE
     C*** End Chg *** 08/05/13 ****************************************
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      @@RRN2        ##RRN2            9 0          FMCMP FILE
     C*** Beg Add *** 08/05/13 ****************************************
     C                   PARM                    xxicd10
     C*** End Add *** 08/05/13 ****************************************
     C*
     C*  DID NOT MAKE A SELECTION
     C*
     C     ##RTCD        IFEQ      '1'
     C*
     C                   SETON                                        3390      STAY ON FIELD
     C*
     C                   ELSE
     C*
     C                   SETON                                        3490      GO TO NEXT FLD
     C*
     C                   ENDIF
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PDA01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  DAICD9        SCICD9
     C                   MOVEL(P)  DALOST        SCLOST
     C*
     C                   MOVEL(P)  DAICD9        KKICD9
     C                   MOVEL(P)  DALOST        KKLOST
     C*
     C     DIKEY         CHAIN     PDI01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  DIDSC1        SCDSC1
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  @@RRN1 NE 0
     C*
     C     ##ICD9        IFNE      *BLANK
     C*
     C                   MOVEL(P)  ##ICD9        SCICD9
     C                   MOVEL(P)  ##LOST        SCLOST
     C                   MOVEL(P)  ##ICD9        KKICD9
     C                   MOVEL(P)  ##LOST        KKLOST
     C*
     C*** Beg Chg *** 08/05/13 ****************************************
      /Free
       If xxicd10 ='1' and scICdInd = 'Y';
          scDSc1 = *blanks;
          kkicd10= ##icd9;
          Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
               scDSc1 = ICdsc1;
            Endif;
       else;
      /End-Free
     C*** End Chg *** 08/05/13 ****************************************
     C     DIKEY         CHAIN     PDI01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  DIDSC1        SCDSC1
     C*
     C                   ENDIF
     C*
     C*** Beg Chg *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Chg *** 08/05/13 ****************************************
     C                   ENDIF                                                  ##ICD9 NE 0
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C                   ENDSR
     C*================================================================
     C* KNDO - F14=DARTS
     C*================================================================
     C     KNDO          BEGSR
     C*
     C                   EXSR      DATCVT
     C*
     C*
     C* CALL API TO ACCESS DARTS
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
     C*
     C                   CALL      'FA703R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM      *BLANKS       ##WCIN            1
     C                   PARM      *BLANKS       ##WCDK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANK        ##KEY            40
     C*
     C                   SETON                                        90
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      *BLANKS       SCMSCD
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCSEQ#
     C                   Z-ADD     0             SCBGDT
     C                   Z-ADD     0             SCENDT
     C                   MOVE      *BLANKS       SCSTS
     C                   MOVE      *BLANKS       SCSDSC
     C                   MOVE      *BLANKS       SCSTAF
     C                   MOVE      *BLANKS       SCNAME
     C                   MOVE      *BLANKS       SCICD9
     C                   MOVE      *BLANKS       SCLOST
     C                   MOVE      *BLANKS       SCDSC1
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   MOVEL     CMCMNU        SCSTAF
     C*
     C                   MOVE      CMSUB#        KKSUB#
     C                   MOVE      CMFND         KKFND
     C                   MOVE      CMFYR         KKFYR
     C                   MOVE      CMCASE        KKCASE
     C                   MOVE      *HIVAL        KKSEQ#
     C*
     C     MSKY1         SETGT     PMS01
     C     MSKY2         READPE    PMS01                                  99
     C*
     C     *IN99         IFEQ      '0'
     C     MSSEQ#        ADD       1             SCSEQ#
     C                   ELSE
     C                   Z-ADD     1             SCSEQ#
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL     MSMSCD        SCMSCD
     C*
     C                   MOVEL     SCMSCD        KKKEY
     C     MCKY          CHAIN     PMC01                              99
     C*
     C     *IN99         IFEQ      '0'
     C                   MOVEL     MCDESC        SCDESC
     C                   ENDIF
     C*
     C                   MOVE      MSSEQ#        SCSEQ#
     C*
     C                   Z-ADD     MSBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCBGDT
     C*
     C                   Z-ADD     MSENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCENDT
     C*
     C                   MOVEL     MSSTS         SCSTS
     C*
     C                   MOVEL     SCSTS         KKCKEY
     C     CLKY          CHAIN     PCL01                              99
     C*
     C     *IN99         IFEQ      '0'
     C                   MOVEL     CLDESC        SCSDSC
     C                   ENDIF
     C*
     C                   MOVEL     MSSTAF        SCSTAF
     C*
     C                   MOVEL     SCSTAF        KKSTAF
     C     UPKEY         CHAIN     PUP01                              99
     C*
     C     *IN99         IFEQ      '0'
     C                   MOVEL     UPUSNM        SCNAME
     C                   ENDIF
     C*
     C                   MOVEL     MSICD9        SCICD9
     C                   MOVEL     MSLOST        SCLOST
     C*
     C                   MOVEL     SCICD9        KKICD9
     C                   MOVEL     SCLOST        KKLOST
     C*
     C     DIKEY         CHAIN     PDI01                              99
     C*
     C     *IN99         IFEQ      '0'
     C                   MOVEL     DIDSC1        SCDSC1
     C                   ENDIF
     C*
     C*
     C                   ENDIF                                                  *IN40 EQ *ON
     C*** Beg Add *** 05/23/13 ****************************************
      /Free
         // Beg Del *** 08/05/13 ****************************************
         //scICdind= 'N';
         // End Del *** 08/05/13 ****************************************
         If xxicd10 = *on and MSicdind ='Y';
            scICd9   = MSicd1001;
            scICdind = MSicdind;
            kkICd10  = MSicd1001;
            scDSc1 = *blanks;
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1) and ICstat ='A';
               scDSc1 = ICdsc1;
            Endif;
         // Beg Add *** 08/05/13 ****************************************
         Elseif MSicdind ='N';
            scICdind = MSicdind;
         // End Add *** 08/05/13 ****************************************
         Endif;
      /End-Free
     C*** End Add *** 05/23/13 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     CMCO#         MSCO#
     C                   MOVE      CMSUB#        MSSUB#
     C                   MOVE      CMFND         MSFND
     C                   MOVE      CMFYR         MSFYR
     C                   MOVE      CMCASE        MSCASE
     C                   MOVEL     SCMSCD        MSMSCD
     C                   MOVE      SCSEQ#        MSSEQ#
     C*
     C                   Z-ADD     SCBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MSBGDT
     C*
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MSENDT
     C*
     C                   MOVEL     SCSTS         MSSTS
     C                   MOVEL     SCSTAF        MSSTAF
     C                   MOVEL     SCICD9        MSICD9
     C                   MOVEL     SCLOST        MSLOST
     C*
     C     MSENDT        IFNE      *ZEROS
     C                   MOVE      'C'           MSSTAT
     C                   ELSE
     C                   MOVE      'O'           MSSTAT
     C                   ENDIF
     C*
     C*** Beg Add *** 05/23/13 ****************************************
      /Free
         MSicdind  = scICdind;
         If xxICD10 = '1' and scICdInd ='Y';
            MSICD1001 = scICd9;
            MSICD9 = *Blanks;
         Endif;
      /End-Free
     C*** End Add *** 05/23/13 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C     *INKJ         CASEQ     *ON           KJDO
     C     *INKN         CASEQ     *ON           KNDO
     C                   ENDCS
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
     C     @@MODE        COMP      'A'                                    40
     C     @@MODE        COMP      'D'                                    09
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
      *
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
     C                   PARM                    ##AR              1            MC AR INST
     C                   PARM                    ##NTWK            1            NETWORK INST
     C                   PARM                    ##UR              1            PRECERT INST
     C                   PARM                    ##MED             1            MEDMGMT INST
     C                   PARM                    ##EDI             1            EDI INST
     C                   PARM                    ##WCDK            1            WC DARTS KEY
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C* GET CLAIM MASTER RECORD,
     C*
     C     @@RRN2        CHAIN     PCM01                              99
     C*
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        ##RTCD
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C     SCL1HD        PARM      *BLANK        ##L1             78
     C     SCL2HD        PARM      *BLANK        ##L2             78
     C*
     C*** Beg Add *** 05/23/13 ****************************************
      /Free

          // Call System API to check wheather the ICD10 is active or not

         @@rtcd = *blanks;
         Callp SA015R(@@rtcd) ;

         Select;
          // ICD10 is inactive
         When @@rtcd = '1';
         xxicd10 = '0';
         DspIcdInd = '1';

          // ICD10 is active
         When @@rtcd = '2';
         xxicd10 = '1';
         DspIcdInd = '0';
         // Beg Add *** 08/05/13 ****************************************
         scICdInd ='Y';
         // End Add *** 08/05/13 ****************************************
         Endsl;

         If @@MODE = 'D';
            DspicdInd = '1';
         Endif;

      /End-Free
     C*** End Add *** 05/23/13 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   TIME                    DFDTTM
     C                   MOVEL     Q1USER        MSUSER
     C                   MOVEL     Q1PGM         MSPGM
     C                   MOVEL     DSTIME        MSTIME
     C*
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MSDT
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        MSUSER
     C                   MOVEL     Q1USER        MSIUSR
     C                   MOVEL     Q1PGM         MSPGM
     C                   MOVEL     Q1PGM         MSIPGM
     C                   MOVEL     DSTIME        MSTIME
     C                   MOVEL     DSTIME        MSITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MSDT
     C                   MOVEL     SRDTO         MSIDT
     C*
     C                   WRITE     DBRCDF
     C*
     C                   MOVE      '1'           OUTFLG
     C*
     C                   ENDSR
     C*
