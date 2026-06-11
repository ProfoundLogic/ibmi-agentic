     Hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*  Program: F0351R                                              *
     H*  Description: Pharmacy Header Info update/display             *
     H*  Created: 04/01/98  C:56 Project: MC152                       *
     H*****************************************************************
     H*  REVISIONS                                                    *
     H*                                                               *
     H*  07/18/25 C:342 Project MC451                                 *
     H*                 NCCI Med Data Call Enhancements               *
     H*                                                               *
     H*  05/23/13 C:079 Project: 46WHE                                *
     H*                 Display Phar Tax field for client 46          *
     H*
     H*  12/21/12 C:277 PROJECT: 146MFP                               *
     H*                 Add processing for field EPRNLN and EPRNFN    *
     H*                 Field SCRNLN and SCRNFN visible only for 146  *
     H*                                                               *
     H*  11/15/12 C:287 Project: MC367
     H*                 Disable function code F11 for GS access to Bill Entry
     H*                                                               *
     H*  04/05/11 C:257 Support 71931                                 *
     H*                 Assigning RRN to @@RRN after WRITEDBRCDF to   *
     H*                 resolve duplicate records in file FAEHP.      *
     H*                                                               *
     H*  04/05/11 C:198 Project: 45MHZ                                *
     H*                 Added Prescribing phy name.                   *
     H*                                                               *
     H*  09/29/10 C:257 PROJECT: 133MJN                               *
     H*                 ADD PHARMACY TAX FIELD AND TAX INCLUDE FLAG   *
     H*                                                               *
     H*  04/18/07 C:188 PROJECT:FL061                                 *
     H*           SET EDITST TO ADJ OR POLICY STATE BASED ON EOCPST   *
     H*                                                               *
     H*  02/21/02 C:062 PROJECT: MC250                                *
     H*                 REQUIRE PHARMACY NUMBER IF FLORIDA BILL.      *
     H*                                                               *
     H*  07/06/01 C:079 PROJECT: FSMNT #196                           *
     H*                 ADD EDIT TO SPECIALTY CODE                    *
     H*                                                               *
     H*  05/30/01 C:062 PROJECT: FS229                                *
     H*                 POPULATE EOMB# SCREEN FIELD                   *
     H*                                                               *
     H*  05/24/01 C:062 Support MCMNT #190                            *
     H*                 COMPILE ONLY                                  *
     H*                                                               *
     H*  10/13/00 C:062 Support MC238                                 *
     H*                 ADD LOGIC TO DEAL WITH NON-SEARCHABLE FIELDS. *
     H*                                                               *
     H*  02/21/00  C:117  PROJ: FS208                                 *
     H*                   ADDED LOGIC TO PROTECT SPECIALTY IF ON A    *
     H*                   RECON; ALSO SAVES SPECIALTY.                *
     H*                                                               *
     H*  11/12/99 C:79  MC152                                         *
     H*                 Delete/Change efficiency isssue.              *
     H*                                                               *
     H*  09/10/99 C:56  Project: KY010                                *
     H*                 Write SCEDI to EOTAPE                         *
     H*                                                               *
     H*****************************************************************
     H*
     FF0371F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFAEPP     UF A E             DISK
     F                                     RENAME(PEP01:DBRCDF)
     F*
     F*  EOMB Transaction File
     FFAEOP     UF   E             DISK
     F*
     F*  WCMC Control File
     FFMCXL     IF   E           K DISK
     F*
     F*  State Code Table File
     FSTSTL     IF   E           K DISK
     F*
     F*  Generic Table File
     FFTABL     IF   E           K DISK
     F*
     F*  Bill Entry Control File
     FFMBEL     IF   E           K DISK
     F*
     F*  Provider Master File
     FFMPDL     IF   E           K DISK
     F*
     F*  EOMB Detail EOB/Notes Transaction File
     FFAEEL     IF   E           K DISK
     FFAEPL     IF   E           K DISK
     F                                     INFDS(DBFDSA)
     F**********BEG ADD 09/29/10 ***********************************************
     FFMP3L2LA  IF   E           K DISK
     F**********END ADD 09/29/10 ***********************************************
     F*** Beg Add *** 07/18/25 ***************************************
     F*  Provider Auxillary File
     FFMP1L     IF   E           K DISK
     F*** End Add *** 07/18/25 ***************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Pharmacy Header Info-
     D                                     rmation')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D*** Beg Add *** 07/18/25 ***************************************
     D invalidPos      s              2  0
      /copy sPrModule
     D*** End Add *** 07/18/25 ***************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          for FAEPP
     C                   PARM                    @@RRN2            9 0          for FAEOP
     C                   PARM                    @@NWCD            5
     C                   PARM                    @@CHRG            9            TOTAL CHARGE
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
     C*  Key List for WCMC Control File
     C*
     C     CXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    ##FND
     C*
     C** Key List for State Code Table File
     C*
     C     STKY          KLIST
     C                   KFLD                    ##KEY
     C*
     C** Key List for Specialty Code File
     C*
     C     SPKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##KEY
     C*
     C** Key List for Fee Schedule Control File
     C*
     C********** SBKY      KLIST
     C**********           KFLD           KKKEY
     C*
     C** Key List for Bill Entry Control FIle
     C*
     C     BEKY          KLIST
     C                   KFLD                    KKAST
     C                   KFLD                    KKSYS
     C                   KFLD                    KKBTYP
     C                   KFLD                    KKSTYP
     C** Key List for Provider Master File
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C* KEY LIST FOR Header EOB/Notes File
     C*
     C     EEKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR EOMB PHYS/HCFA HEADER FILE
     C*
     C     EPKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CXSUB#        ##SUB#
     C     *LIKE         DEFINE    STKEY         ##KEY
     C     *LIKE         DEFINE    EPCO#         KKCO#
     C     *LIKE         DEFINE    EPSUB#        KKSUB#
     C     *LIKE         DEFINE    EPFND         KKFND
     C     *LIKE         DEFINE    EPFYR         KKFYR
     C     *LIKE         DEFINE    EPCASE        KKCASE
     C********** *LIKE     DEFN FBKEY     KKKEY
     C     *LIKE         DEFINE    BEAST         KKAST
     C     *LIKE         DEFINE    BESYS         KKSYS
     C     *LIKE         DEFINE    BEBTYP        KKBTYP
     C     *LIKE         DEFINE    BESTYP        KKSTYP
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C*** BEGIN ADD *** 04/18/07 **************************************
     C     *LIKE         DEFINE    EOAST         EDITST
     C*** END   ADD *** 04/18/07 **************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DATSBR - Date check subrountine
     C*================================================================
     C     DATSBR        BEGSR
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
     C*** BEG ADD ***** 09/29/10 **************************************
     C     XXCLT#        IFEQ      '133'
     C     @@MODE        ANDEQ     'A'
     C     @@MODE        OREQ      'C'
      *** Beg Add ***** 05/23/13 **************************************
     C     EOTAX         ANDEQ     0
     C     EOAST         ANDEQ     'LA'
      *** End Add ***** 05/23/13 **************************************
     C     SCTAXF        IFEQ      'Y'
     C     PDKY          CHAIN     FMP3L2LA                           99
     C     *IN99         IFEQ      *ON
     C                   MOVE      '1'           *IN39
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD ***** 09/29/10 **************************************
     C*** BEG ADD *** 07/06/01 ****************************************
     C*
     C** SPECIALTY CODE MUST BE VALID IF ENTERED AND NOT A RECON
     C*
     C     SCSPCD        IFNE      *BLANKS
     C     *IN29         ANDEQ     *OFF
     C*
     C                   MOVEL(P)  SCSPCD        ##KEY
     C     SPKY          CHAIN     PSP01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9083
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD *** 07/06/01 ****************************************
       //*** Beg add ***** 07/18/25 ***********************************
       If isModEnabled('NCCI Med Data Call');
          If scsvzp = *zeros and EOrcn = 0;
             ersvzp = *on;
             mssvzp = 'MCR2472';
             *in90 = *on;
             leavesr;
          Endif;
       Endif;
       //*** End add ***** 07/18/25 ************************************
     C*** BEGIN ADD *** 02/21/02 **************************************
     C*
     C* PHARMACY NUMBER IS REQUIRED IF FLORIDA BILL
     C*
     C     SCPHAR        IFEQ      *BLANKS
     C*** BEG CHG *** 04/18/07 ****************************************
     C*****      EOAST     ANDEQ'FL'
     C     EDITST        ANDEQ     'FL'
     C*** END CHG *** 04/18/07 ****************************************
     C                   SETON                                        8990
     C                   ENDIF
     C*** END   ADD *** 02/21/02 **************************************
     C*
     C** ELIGIBLE FOR DATA SUBMISSION FEILD IS REQUIRED IF IN30 IS ON
     C*          (DETERMINED IN PGMSTR SUBROUINTE)
     C*
     C   30SCEDI         IFEQ      *BLANKS
     C                   SETON                                        9078
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCEDI IFEQ BLANKS
     C*
     C*
     C*** BEG CHG *** 04/18/07 ****************************************
     C*****      SCEDI     IFNE *BLANKS
     C   30SCEDI         IFNE      *BLANKS
     C*** END CHG *** 04/18/07 ****************************************
     C     SCEDI         ANDNE     'Y'
     C     SCEDI         ANDNE     'N'
     C                   SETON                                        9079
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCEDI IFNE BLANKS
     C*
       //*** Beg add ***** 07/18/25 *************************************
       If isModEnabled('NCCI Med Data Call');
          // %check return non zero if non numeric value including blank
          invalidPos = %check('0123456789': %trim(scdnpi));
          If invalidPos <> 0 or scdnpi = *blanks;
             msdnpi = 'MC50007';
             erdnpi = *on;
             *in90 = *on;
             leavesr;
          Endif;
       Endif;
       //*** End add ***** 07/18/25 ************************************
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
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C                   SETOFF                                       313233
     C                   SETOFF                                       343536
     C                   SETOFF                                       37
     C*
     C*** CLEAR HOLD FIELDS
     C*
     C                   SELECT
     C*
     C*
     C*** Beg Add *** 07/18/25 ***************************************
     C     HFLD          WHENEQ    'SCSVZP'
     C                   EVAL       *in90  =*on
     C                   EVAL       ersvzp =*on
     C                   EVAL       mssvzp ='MCR0676'
     C     HFLD          WHENEQ    'SCDNPI'
     C                   EVAL       *in90  =*on
     C                   EVAL       erdnpi = *on
     C                   EVAL       msdnpi = 'MCR0676'
     C*** End Add *** 07/18/25 ***************************************
     C     HFLD          WHENEQ    'SCSPCD'
     C                   MOVEL     SCSPCD        HLSPCD            3
     C                   CLEAR                   ##KEY1
     C                   CALL      'F2335R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##KEY1           10
     C                   PARM      ' '           ##RTCD
     C*
     C     ##RTCD        IFNE      '*'
     C                   MOVE      HLSPCD        SCSPCD
     C                   SETON                                        9036      pos cursor
     C                   ELSE
     C                   MOVEL     ##KEY1        SCSPCD
     C                   MOVEL     SCSPCD        ##KEY
     C     SPKY          CHAIN     PSP01                              99
     C  N99              MOVEL     SPDESC        SCSCDS
     C                   SETON                                        9037      pos cursor
     C                   ENDIF
     C*** BEGIN ADD *** 10/13/00 **************************************
     C                   OTHER
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCPHAR'
     C                   SETON                                        51
     C     HFLD          WHENEQ    'SCEDI'
     C                   SETON                                        52
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*** END   ADD *** 10/13/00 **************************************
     C*
     C                   ENDSL
     C*
     C                   EXSR      DATSBR
     C*
     C                   ENDSR
     C*================================================================
     C* KKDO - F11=EOB/Notes
     C*================================================================
     C     KKDO          BEGSR
     C                   Z-ADD     0             ##RRN
     C                   Z-ADD     @@RRN2        ##RRN2            9 0
     C                   CALL      'F0355R'
     C                   PARM      ' '           ##CALL
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##RRN
     C                   PARM                    ##RRN2
     C                   SETON                                        90
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   CLEAR                   SCCHRG
     C                   CLEAR                   SCMRA
     C                   CLEAR                   SCNTWK
     C                   CLEAR                   SCOTHR
     C                   CLEAR                   SCOTH2
     C                   CLEAR                   SCPAID
     C*
     C* If in ADD mode and @@RRN is not zero, then user has used F12
     C* from the detail program.  This require a chain to the
     C* Header Info file in order to access the Header information.
     C*
     C     @@MODE        IFEQ      'A'
     C     @@RRN         ANDNE     *ZEROS
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   ENDIF                                                  @@MODE = 'A'
     C*
     C** GET PROVIDER SPECIALTY CODE (SERVICING PROVIDER)
     C*
     C                   MOVE      EOFEIN        KKFEIN
     C                   MOVE      EOMOD         KKMOD
     C*** BEG CHG *** 02/21/00 **(PUT CONDITION AROUND SCSPCD)*********
     C*  IF THERE IS A SPECIALTY CODE IN THE FAEOP FILE, USE IT;
     C*    OTHERWISE, USE THE ON IN FMPDP
     C     EOSPCD        IFNE      *BLANKS
     C                   MOVEL     EOSPCD        SCSPCD
     C                   ELSE
     C     PDKY          CHAIN     PPD01                              99
     C  N99              MOVEL     PDSPCD        SCSPCD
     C                   ENDIF
     C*** END CHG *** 02/21/00 **(PUT CONDITION AROUND SCSPCD)*********
     C*
     C                   MOVEL     SCSPCD        ##KEY
     C     SPKY          CHAIN     PSP01                              99
     C  N99              MOVEL     SPDESC        SCSCDS
     C*
     C                   MOVEL     EOAST         SCAST
     C                   MOVE(P)   EOZIPA        SCZIPA

     C*** Beg Add *** 07/18/25 ***************************************
     C     EOFLIC        IFNE      *BLANKS
     C                   MOVEL     EOFLIC        SCSVZP
     C                   MOVE      EOFLIC        SCZIP
     C                   ELSE
     C     PDKY          CHAIN     PP101                              99
     C  N99              MOVEL     P1ZIP         SCSVZP
     C  N99              MOVE      P1ZIP         SCZIP
     C                   ENDIF
     C*** End Add *** 07/18/25 ***************************************
     C*
     C*****      @@MODE    IFNE 'A'
     C*
     C*
     C* DETERMINE IF ANY EOB/NOTES EXISTS FOR THIS BILL
     C*
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C     EEKY          CHAIN     PEE01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        68
     C                   ELSE
     C                   SETOFF                                       68
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C*
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVEL     EPDNPI        SCDNPI
     C*** End Add *** 07/18/25 ***************************************
     C*
     C     EPEDI         IFNE      *BLANKS
     C                   MOVE      EPEDI         SCEDI
     C                   ENDIF                                                  EPEDI NE *BLANKS
     C                   MOVEL     EPPHAR        SCPHAR
     C*
     C                   Z-ADD     EOCHRG        SCCHRG
     C                   Z-ADD     EOMRA         SCMRA
     C                   Z-ADD     EONDIS        SCNTWK
     C                   Z-ADD     EOOTHR        SCOTHR
     C                   Z-ADD     EOOTH2        SCOTH2
     C                   Z-ADD     EOPAID        SCPAID
     C*
     C*****                ENDIF                           @@MODE NE 'A'
     C*
     C*** BEG ADD ***** 09/29/10 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
      *** Beg Chg ***** 05/23/13 **************************************
     C**** XXCLT#        IFEQ      '133'
     C     EOAST         IFEQ      'LA'
      *** End Chg ***** 05/23/13 **************************************
     C     EPTAX         IFEQ      ' '
     C                   MOVE      'N'           SCTAXF
     C                   ELSE
     C                   MOVEL     EPTAX         SCTAXF
     C                   ENDIF
     C*
     C     EOTAX         IFNE      *ZERO
     C                   MOVEL     EOTAX         SCTAX
     C                   ELSE
     C                   MOVEL     *ZERO         SCTAX
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD ***** 09/29/10 **************************************
     C*** Beg Add ***** 04/05/11 **************************************
     C                   MOVEL     EPPNAM        SCPNAM
     C*** End Add ***** 04/05/11 **************************************
     C*** Beg Add *** 12/21/12 ****************************************
      /Free
       If DspClt146 = *On;
         scRnln = EPrnln;
         scRnfn = EPrnfn;
       Endif;
      /End-Free
     C*** End Add *** 12/21/12 ****************************************
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     EOCO#         EPCO#
     C                   Z-ADD     EOSUB#        EPSUB#
     C                   Z-ADD     EOFND         EPFND
     C                   Z-ADD     EOFYR         EPFYR
     C                   Z-ADD     EOCASE        EPCASE
     C                   MOVE      EOSS#         EPSS#
     C                   Z-ADD     EOACDT        EPACDT
     C*
     C                   MOVE      SCEDI         EPEDI
     C                   MOVE      SCPHAR        EPPHAR
     C*** BEG ADD *** 02/21/00 ****************************************
     C                   MOVE      SCSPCD        EOSPCD
     C*** END ADD *** 02/21/00 ****************************************
     C*** Beg Add *** 07/18/25 ***************************************
     C     SCSVZP        IFEQ      *ZEROS
     C                   MOVEL     '     '       EOFLIC
     C     PDKY          CHAIN     PP101                              99
     C  N99              MOVEL     P1ZIP         SCSVZP
     C  N99              MOVE      P1ZIP         SCZIP
     C                   ENDIF
     C                   MOVEL     SCSVZP        EOFLIC
     C                   MOVE      SCZIP         EOFLIC
     C                   MOVEL     SCDNPI        EPDNPI
     C*** End Add *** 07/18/25 ***************************************
     C*
     C*****                Z-ADDSCCHRG    EOCHRG
     C*
     C*** BEG ADD ***** 09/29/10 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#                          3
      *** Beg Chg ***** 05/23/13 **************************************
     C**** XXCLT#        IFEQ      '133'
     C     EOAST         IFEQ      'LA'
      *** End Chg ***** 05/23/13 **************************************
     C                   MOVE      SCTAXF        EPTAX
     C                   ENDIF
     C*
     C*** END ADD ***** 09/29/10 **************************************
     C*** Beg Add ***** 04/05/11 **************************************
     C                   MOVEL     SCPNAM        EPPNAM
     C*** End Add ***** 04/05/11 **************************************
     C*** Beg Add *** 12/21/12 ****************************************
      /Free
       If DspClt146 = *On;
         EPrnln = scRnln;
         EPrnfn = scRnfn;
       Endif;
      /End-Free
     C*** End Add *** 12/21/12 ****************************************
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
       //*** Beg add ***** 07/18/25 ***********************************
       ersvzp = *off;
       mssvzp = *blanks;
       erdnpi = *off;
       msdnpi = *blanks;
       //*** End add ***** 07/18/25 ***********************************
     C     *INKJ         CASEQ     *ON           KJDO                           F10=search
     C                   ENDCS
     C*** Beg Add *** MC367 *** 11/15/12 ******************************
      /Free
      /If Defined(WEB)
         If *INKK;
           ExSr KKDO;  //F11=EOB/Notes
         EndIf;
      /EndIf
      /End-Free
     C*** End Add *** MC367 *** 11/15/12 ******************************
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*  Get relative record number for header record and put in
     C*  parm @@RRN.  This will allow the ability to access header
     C*  data if user F12=Cancel from detail screen on an add.
     C*
     C     @@MODE        IFEQ      'A'
     C     EPKY          CHAIN     PEP01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRNA        @@RRN
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  @@MODE = 'A'
     C*
     C* The following call to the WRITER subroutine is necessary
     C* because if program bombs out in add mode before the user
     C* takes action to write the header record, 'change' mode
     C* will be looking to update a record that does not exist.
     C* Thus, if the @@RRN (FAEUP) is blanks in change mode, we must
     C* call the write subroutine to add the header record.
     C*       (WILL BE EXECUTED IF USER HITS ENTER)
     C*
     C     @@MODE        IFEQ      'C'
     C     *INKC         ANDEQ     *OFF
     C     @@RRN         ANDEQ     *ZEROS
     C                   EXSR      WRITER
     C                   ENDIF                                                  @@MODE = 'C'
     C*
     C*
     C*   Call Cancel/Pend program if user hits F3=EXIT
     C*   If in Add mode, enable user to cancel bill (##CANC = Y)
     C*   Otherwise do not allow cancel capability (##CANC = N)
     C*
     C     *INKC         IFEQ      *ON
     C     @@MODE        ANDNE     'D'
     C*
     C     @@MODE        IFEQ      'A'
     C                   MOVE      'Y'           ##CANC                         able to cancel
     C                   ELSE
     C                   MOVE      ' '           ##CANC                         unable to cancel
     C                   ENDIF
     C*
     C* If in add mode and F3=Exit is hit, call writer subroutine
     C* to write the header record
     C*
     C     @@MODE        IFEQ      'A'
     C     *INKC         ANDEQ     *ON
     C                   EXSR      WRITER
     C                   ENDIF                                                  @@MODE = 'A'
     C*
     C* If in 'change' mode and F3=Exit is hit, call update subroutine
     C* to update the header record with the changed info.
     C* This will work on both SCR01 and SCR02.
     C*
     C     @@MODE        IFEQ      'C'
     C     *INKC         ANDEQ     *ON
     C     @@RRN         ANDNE     *ZEROS
     C                   EXSR      UPDATR
     C                   ENDIF                                                  @@MODE = 'C'
     C*
     C* The following call to the WRITER subroutine is necessary
     C* because if program bombs out in add mode before the user
     C* takes action to write the header record, 'change' mode
     C* will be looking to update a record that does not exist.
     C* Thus, if the @@RRN (FAEUP) is blanks in change mode, we must
     C* call the write subroutine to add the header record.
     C*       (WILL BE EXECUTED IF USER HITS F3=EXIT)
     C*
     C     @@MODE        IFEQ      'C'
     C     @@RRN         ANDEQ     *ZEROS
     C                   EXSR      WRITER
     C                   ENDIF                                                  @@MODE = 'C'
     C*
     C                   Z-ADD     @@RRN2        ##RRN
     C                   CALL      'F0392R'
     C     *INKL         PARM      ' '           ##KL
     C                   PARM      ' '           ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM                    ##CANC            1
     C                   PARM                    @@CHRG
     C                   PARM      ' '           ##RTCD            1
     C**********           MOVE '1'       @@KC
     C*
     C                   ENDIF                                                  *INKC = *ON
     C*
     C*** BEG DEL *** 11/12/99 ****************************************
     C* CALL 'ADJUDICATION CALL' PROGRAM IF IN UNPEND MODE
     C* THIS IS DONE BECAUSE OTHER BILLS MAY HAVE BEEN PROCOSSED
     C* FOR THIS CASE SINCE THIS BILL HAS BEEN ADJUDICATED
     C*********
     C*********  EOBLST    IFEQ 'U'
     C*********  *INKL     ANDNE*ON
     C*********  ##RTCD    ANDNE'P'
     C*********            Z-ADD@@RRN2    ##RRN
     C*********            CALL 'FA713R'
     C*********            PARM ' '       ##KC
     C*********            PARM ' '       ##KL
     C*********            PARM           ##RRN
     C*********            ENDIF                           EOBLST = 'U'
     C*** END DEL *** 11/12/99 ****************************************
     C*
     C                   SETON                                        LR
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C** Get standard screen headings
     C*
      *** BEG ADD ***** 09/29/10 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#                          3
      *** Beg Chg ***** 05/23/13 **************************************
     C**** XXCLT#        IFNE      '133'
     C     EOAST         IFNE      'LA'
      *** End Chg ***** 05/23/13 **************************************
     C                   MOVE      *ON           *IN38
      *** Beg Add ***** 05/23/13 **************************************
     C                   ELSE
     C                   MOVE      *OFF          *IN38
      *** End Add ***** 05/23/13 **************************************
     C                   ENDIF
      *** END ADD ***** 09/29/10 **************************************
     C*** Beg Add *** 12/21/12 ****************************************
      /Free
         If XXCLT# = '146';
            DspClt146 = *On;
         Else;
            DspClt146 = *Off;
         Endif;
      /End-Free
     C*** End Add *** 12/21/12 ****************************************
     C     @@RRN2        CHAIN(N)  PEO01                              99
     C*
     C*** BEG ADD *** 04/18/07 ****************************************
     C*
     C*  SET EDITST TO POLICY OR ADJ STATE BASED ON CPST FLAG
     C*
     C     EOCPST        IFEQ      'Y'
     C                   MOVEL(P)  EOPOST        EDITST
     C                   ELSE
     C                   MOVEL(P)  EOAST         EDITST
     C                   ENDIF
     C*
     C*** END ADD *** 04/18/07 ****************************************
     C*
     C*** BEGIN ADD *** 05/30/01 **************************************
     C                   Z-ADD     EOEOB#        SCEOB#
     C*** END   ADD *** 05/30/01 **************************************
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
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
     C* CALL API FOR SYSTEM FLAGS
     C*
     C                   CALL      'SA001R'
     C                   PARM      ' '           ##WCIF            1
     C                   PARM      ' '           ##MCIF            1
     C                   PARM      ' '           ##WCIN            1
     C                   PARM      ' '           ##AR              1
     C                   PARM      ' '           ##NTWK            1
     C                   PARM      ' '           ##UR              1
     C                   PARM      ' '           ##MED             1
     C                   PARM      ' '           ##EDI             1
     C                   PARM      ' '           ##WCDK            1
     C                   PARM      ' '           ##RHAB            1
     C*
     C* SET INDICATOR TO DISPLAY NEWORK REDUCTIONS FIELD IF
     C* NETWORK IS INSTALLED
     C*
     C     ##NTWK        IFEQ      'Y'
     C                   SETON                                        47
     C                   ELSE
     C                   SETOFF                                       47
     C                   ENDIF                                                  ##NTWK = 'Y'
     C*
     C** Get 'Reductions' headings
     C*
      /Free
       clear scHMRA_dot;
       clear scHOT1_dot;
      /If Not Defined(WEB)
       scHMRA_dot = '. . . . . .';
       scHOT1_dot = '. . . . . .';
      /Endif
      /End-free
     C                   Z-ADD     EOSUB#        ##SUB#
     C     CXKY          CHAIN     PCX01                              99
     C                   MOVEL     CXHMRA        SCHMRA
     C                   MOVEL     CXHNET        SCHNET
     C                   MOVEL     CXHOT1        SCHOT1
     C                   MOVEL     CXHOT2        SCHOT2
     C*
     C** DETERMINE IF ELIGIBLE FOR DATA SUBMISSION SHOULD BE DISPLAYED
     C** AND REQUIRED
     C*
     C*** BEG CHG *** 04/18/07 ****************************************
     C*****                MOVELEOAST     ##KEY
     C                   MOVEL     EDITST        ##KEY
     C*** END CHG *** 04/18/07 ****************************************
     C     STKY          CHAIN     PST01                              99
     C     *IN99         IFEQ      *OFF
     C     ST1A4         ANDEQ     'Y'
     C                   SETON                                        30
     C*** BEG CHG *** 04/18/07 ****************************************
     C*****                MOVE EOAST     KKAST
     C                   MOVE      EDITST        KKAST
     C*** END CHG *** 04/18/07 ****************************************
     C                   MOVE      EOSYS         KKSYS
     C                   MOVE      EOBTYP        KKBTYP
     C                   MOVEL     EOSTYP        KKSTYP
     C*
     C     BEKY          CHAIN     PBE01                              98
     C  N98              MOVE      BEEDI         SCEDI
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       30
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C*
     C** SET VARIOUS FIELDS
     C*
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C*
     C* DISPLAY PROVIDER NETWORK IF NOT BLANKS
     C*
     C     EONTWK        IFNE      *BLANKS
     C                   MOVEL(P)  EONTWK        SCNWCD
     C                   SETON                                        69
     C                   ELSE
     C                   SETOFF                                       69
     C                   ENDIF                                                  EONTWK
     C*
     C* SET INITIAL CURSOR POSITON TO PHARMACY NUMBER
     C                   SETON                                        37
     C*** BEG ADD *** 02/21/00 ****************************************
     C*
     C* PROTECT SPECIALTY IF EORCN>0
     C*
     C     EORCN         IFGT      *ZERO
     C                   SETON                                        29
     C                   ELSE
     C*
     C                   SETOFF                                       29
     C                   ENDIF                                                  EORCN>0
     C*** END ADD *** 02/21/00 ****************************************
     C*** BEGIN ADD *** 02/21/02 **************************************
     C*
     C* Make pharmacy number required if Florida Bill
     C*
     C                   SETOFF                                       16
     C*** BEG CHG *** 04/18/07 ****************************************
     C*****      EOAST     IFEQ 'FL'
     C     EDITST        IFEQ      'FL'
     C*** END CHG *** 04/18/07 ****************************************
     C                   SETON                                        16
     C                   ENDIF
     C*
     C*** END   ADD *** 02/21/02 **************************************
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
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1PGM         EPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EPTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C*
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C*
     C** UPDATE EOMB TRANSACTION FILE
     C*
     C     @@RRN2        CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  SCSPCD        EOSPCD
     C*** BEG ADD *** 09/10/99 ****************************************
     C                   MOVE      SCEDI         EOTAPE
     C*** END ADD *** 09/10/99 ****************************************
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     SRDTO         EODT
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVEL     SCSVZP        EOFLIC
     C                   MOVE      SCZIP         EOFLIC
     C*** End Add *** 07/18/25 ***************************************
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C* If in ADD mode and @@RRN (FAEPP) is not zeros, then the user
     C* has used F12 from detail screen.  Must delete previous written
     C* Header record and write the new one.  This prevents having two
     C* Header records for the bill.
     C*
     C     @@RRN         IFNE      *ZEROS
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   DELETE    DBRCDF
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  @@RRN NE *ZEROS
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   Z-ADD     EOEOB#        EPEOB#
     C                   MOVE      'A'           EPSTAT
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1USER        EPIUSR
     C                   MOVEL     Q1PGM         EPPGM
     C                   MOVEL     Q1PGM         EPIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EPTIME
     C                   Z-ADD     DSTIME        EPITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C                   MOVEL     SRDTO         EPIDT
     C*
     C                   WRITE     DBRCDF
     C*** BEG ADD *** 04/05/11 ****************************************
     C     EPKY          CHAIN     PEP01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRNA        @@RRN
     C                   ENDIF                                                  *IN99 = *OFF
     C*** END ADD *** 04/05/11 ****************************************
     C     @@RRN2        CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C******               Z-ADDSCCHRG    EOCHRG
     C                   MOVEL(P)  SCSPCD        EOSPCD
     C*** BEG ADD *** 09/10/99 ****************************************
     C                   MOVE      SCEDI         EOTAPE
     C*** END ADD *** 09/10/99 ****************************************
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     SRDTO         EODT
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVEL     SCSVZP        EOFLIC
     C                   MOVE      SCZIP         EOFLIC
     C*** End Add *** 07/18/25 ***************************************
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C                   FEOD      FAEPP
     C*
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
