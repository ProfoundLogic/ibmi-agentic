     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*  Program: F0451R                                              *
     H*  Description: Physician (HCFA) Header Info update/display     *
     H*  Created: 03/08/00  C:53 Project: KY010                       *
     H*****************************************************************
     H*  REVISIONS                                                    *
     H*
     H*  04/23/07 C:188 PROJECT:FL061
     H*                 SET EDITST TO ADJ OR POLICY STATE BASED ON EOCPST
     H*
     H*  03/12/03 C:079 SUPPORT: MC044 36319
     H*                 CHECK THE STATUS OF DIAG CODES
     H*
     H*  11/22/00  C:062  Proj: FS212
     H*                   COMPILE ONLY.
     H*
     H*  02/21/00  C:117  PROJ: FS208
     H*                   ADDED LOGIC TO PROTECT SPECIALTY IF ON A
     H*                   RECON; ALSO SAVES SPECIALTY.
     H*
     H*  02/16/00 C:53  Project: KY010
     H*                 ADD KY REQUIRED FIELD.
     H*
     H*  11/12/99 C:79  MC152
     H*                 Delete/Change efficiency isssue.
     H*                                                               *
     H*  09/10/99 C:56  Project: KY010                                *
     H*                 Write SCEDI to EOTAPE                         *
     H*                                                               *
     H*  07/13/99/C:56  Project: MC152                                *
     H*                 Call Diagnosis Code search only for 'D' LOS   *
     H*                 type.                                         *
     H*****************************************************************
     H*
     FF0451F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFAEHP     UF A E             DISK
     F                                     RENAME(PEH01:DBRCDF)
     F*
     F*  EOMB Transaction File
     FFAEOP     UF   E             DISK
     F*
     F*  WCMC Control File
     FFMCXL     IF   E           K DISK
     F*
     F*  Diagnosis Code Physical File
     FFMDIL     IF   E           K DISK
     F*
     F*  State Code Table File
     FSTSTL     IF   E           K DISK
     F*
     F*  Generic Table File
     FFTABL     IF   E           K DISK
     F*
     F*  Fee Schedule Base Control File
     F*SSYLF   IF  E           K        DISK
     F*           SSY01                             KIGNORE
     F*           SPC01                             KIGNORE
     F*           SWC01                             KIGNORE
     F*           SRC01                             KIGNORE
     F*           SDR01                             KIGNORE
     F*           SBC01                             KIGNORE
     F*           SCS01                             KIGNORE
     F*           SGL01                             KIGNORE
     F*           SP201                             KIGNORE
     F*
     F*  Bill Entry Control File
     FFMBEL     IF   E           K DISK
     F*
     F*  Provider Master File
     FFMPDL     IF   E           K DISK
     F*
     F*  EOMB Detail EOB/Notes Transaction File
     FFAEEL     IF   E           K DISK
     FFAEHL     IF   E           K DISK
     F                                     INFDS(DBFDSA)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Physician (HCFA) Hea-
     D                                     der Information')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          for FAEHP
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
     C*  Key List for Diagnosis Code File
     C*
     C     DIKY          KLIST
     C                   KFLD                    KKICD9
     C                   KFLD                    KKLOST
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
     C     EHKY          KLIST
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
     C     *LIKE         DEFINE    DIICD9        ##ICD9
     C     *LIKE         DEFINE    DILOST        ##LOST
     C     *LIKE         DEFINE    DIICD9        KKICD9
     C     *LIKE         DEFINE    DILOST        KKLOST
     C     *LIKE         DEFINE    STKEY         ##KEY
     C     *LIKE         DEFINE    EHCO#         KKCO#
     C     *LIKE         DEFINE    EHSUB#        KKSUB#
     C     *LIKE         DEFINE    EHFND         KKFND
     C     *LIKE         DEFINE    EHFYR         KKFYR
     C     *LIKE         DEFINE    EHCASE        KKCASE
     C********** *LIKE     DEFN FBKEY     KKKEY
     C     *LIKE         DEFINE    BEAST         KKAST
     C     *LIKE         DEFINE    BESYS         KKSYS
     C     *LIKE         DEFINE    BEBTYP        KKBTYP
     C     *LIKE         DEFINE    BESTYP        KKSTYP
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C*** BEG ADD *** 04/23/07 ****************************************
     C     *LIKE         DEFINE    EOAST         EDITST
     C*** END ADD *** 04/23/07 ****************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DATSBR - Date check subrountine
     C*================================================================
     C     DATSBR        BEGSR
     C*
     C* THIS SUBROUTINE IS CALLED AFTER EVERY SEARCH TO PROPERLY
     C* FORMAT ALL DATES ON SCREEN
     C*
     C                   Z-ADD     SCDFC         SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDFC
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
     C** VALIDATE DATE FIRST CONSULTED
     C*
     C     SCDFC         IFNE      *ZEROS
     C                   Z-ADD     SCDFC         SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9071
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCDFC
     C                   ENDIF                                                  SRERR IFEQ 1
     C                   ENDIF                                                  SCDFC IFNE ZEROS
     C*
     C*** DIAGNOSIS CODE ONE IS REQUIRED
     C*
     C     SCDIA1        IFEQ      *BLANKS
     C                   SETON                                        9072
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCDIA1 IFEQ BLANKS
     C*
     C** VALIDATE 1ST DIAGNOSIS CODE
     C*
     C                   MOVE(P)   SCDIA1        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C*** BEG ADD *** 03/12/03 ****************************************
     C     DISTAT        OREQ      'I'
     C*** END ADD *** 03/12/03 ****************************************
     C                   SETON                                        9073
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C** VALIDATE DIAGNOSIS CODE #2 IF ENTERED
     C*
     C     SCDIA2        IFNE      *BLANKS
     C                   MOVE(P)   SCDIA2        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C*** BEG ADD *** 03/12/03 ****************************************
     C     DISTAT        OREQ      'I'
     C*** END ADD *** 03/12/03 ****************************************
     C                   SETON                                        9074
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF                                                  SCDIA2 IFNE BLANKS
     C*
     C** VALIDATE DIAGNOSIS CODE #3 IF ENTERED
     C*
     C     SCDIA3        IFNE      *BLANKS
     C                   MOVE(P)   SCDIA3        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C*** BEG ADD *** 03/12/03 ****************************************
     C     DISTAT        OREQ      'I'
     C*** END ADD *** 03/12/03 ****************************************
     C                   SETON                                        9075
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF                                                  SCDIA2 IFNE BLANKS
     C*
     C** VALIDATE DIAGNOSIS CODE #4 IF ENTERED
     C*
     C     SCDIA4        IFNE      *BLANKS
     C                   MOVE(P)   SCDIA4        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C*** BEG ADD *** 03/12/03 ****************************************
     C     DISTAT        OREQ      'I'
     C*** END ADD *** 03/12/03 ****************************************
     C                   SETON                                        9076
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF                                                  SCDIA2 IFNE BLANKS
     C*
     C** TOTAL CHARGE CANNOT BE ZERO
     C*
     C*****      SCCHRG    IFEQ *ZEROS
     C*****                SETON                     9077
     C*****                GOTO CK1END
     C*****                ENDIF
     C*
     C*** BEG DEL *** 03/08/00 ****************************************
     C***ELIGIBLE FOR DATA SUBMISSION FEILD IS REQUIRED IF IN30 IS ON
     C*******    (DETERMINED IN PGMSTR SUBROUINTE)
     C*******
     C***30**    SCEDI     IFEQ *BLANKS
     C*******              SETON                     9078
     C*******              GOTO CK1END
     C*******              ENDIF                           SCEDI IFEQ BLANKS
     C*******
     C*******    SCEDI     IFNE *BLANKS
     C*******    SCEDI     ANDNE'Y'
     C*******    SCEDI     ANDNE'N'
     C*******              SETON                     9079
     C*******              GOTO CK1END
     C*******              ENDIF                           SCEDI IFNE BLANKS
     C*** END DEL *** 03/08/00 ****************************************
     C*
     C*** BEG ADD *** 02/16/00 ***********************************
     C*
     C*** BEG CHG *** 04/23/07 ***********************************
     C*****      SCAST     IFEQ 'KY'
     C     EDITST        IFEQ      'KY'
     C*** END CHG *** 04/23/07 ***********************************
     C     ##EDI         ANDEQ     'Y'
     C     SCACCT        ANDEQ     *BLANKS
     C                   SETON                                        9080
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCEDI IFNE BLANKS
     C*** END ADD *** 02/16/00 ***********************************
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
     C                   CLEAR                   HLDIA
     C*
     C                   SELECT
     C*
     C** DIAGNOSIS CODE #2 SEARCH
     C*
     C     HFLD          WHENEQ    'SCDIA1'
     C                   MOVE(P)   SCDIA1        HLDIA             5
     C                   CLEAR                   ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##ICD9
     C*** BEG CHG *** 07/13/99 ****************************************
     C**********           PARM ' '       ##LOST
     C                   PARM      'D'           ##LOST
     C*** END CHG *** 07/13/99 ****************************************
     C*
     C     ##KL          IFEQ      '1'
     C     ##ICD9        OREQ      *BLANKS
     C                   MOVE(P)   HLDIA         SCDIA1
     C                   SETON                                        9034      pos cursor
     C                   ELSE
     C                   MOVE(P)   ##ICD9        SCDIA1
     C                   SETON                                        9031      pos cursor
     C                   ENDIF
     C*
     C     SCDIA1        IFNE      *BLANKS
     C                   MOVE(P)   SCDIA1        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD1DS
     C                   ENDIF
     C*
     C** DIAGNOSIS CODE #2 SEARCH
     C*
     C     HFLD          WHENEQ    'SCDIA2'
     C                   MOVE(P)   SCDIA2        HLDIA             5
     C                   CLEAR                   ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##ICD9
     C                   PARM      ' '           ##LOST
     C*
     C     ##KL          IFEQ      '1'
     C     ##ICD9        OREQ      *BLANKS
     C                   MOVE(P)   HLDIA         SCDIA2
     C                   SETON                                        9031      pos cursor
     C                   ELSE
     C                   MOVE(P)   ##ICD9        SCDIA2
     C                   SETON                                        9032      pos cursor
     C                   ENDIF
     C*
     C     SCDIA2        IFNE      *BLANKS
     C                   MOVE(P)   SCDIA2        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD2DS
     C                   ENDIF
     C*
     C** DIAGNOSIS CODE #3 SEARCH
     C*
     C     HFLD          WHENEQ    'SCDIA3'
     C                   MOVE(P)   SCDIA3        HLDIA             5
     C                   CLEAR                   ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##ICD9
     C                   PARM      ' '           ##LOST
     C*
     C     ##KL          IFEQ      '1'
     C     ##ICD9        OREQ      *BLANKS
     C                   MOVE(P)   HLDIA         SCDIA3
     C                   SETON                                        9032      pos cursor
     C                   ELSE
     C                   MOVE(P)   ##ICD9        SCDIA3
     C                   SETON                                        9033      pos cursor
     C                   ENDIF
     C*
     C     SCDIA3        IFNE      *BLANKS
     C                   MOVE(P)   SCDIA3        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD3DS
     C                   ENDIF
     C*
     C** DIAGNOSIS CODE #4 SEARCH
     C*
     C     HFLD          WHENEQ    'SCDIA4'
     C                   MOVE(P)   SCDIA4        HLDIA             5
     C                   CLEAR                   ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##ICD9
     C                   PARM      ' '           ##LOST
     C*
     C     ##KL          IFEQ      '1'
     C     ##ICD9        OREQ      *BLANKS
     C                   MOVE(P)   HLDIA         SCDIA4
     C                   SETON                                        9033      pos cursor
     C                   ELSE
     C                   MOVE(P)   ##ICD9        SCDIA4
     C                   SETON                                        9035      pos cursor
     C                   ENDIF
     C*
     C     SCDIA4        IFNE      *BLANKS
     C                   MOVE(P)   SCDIA4        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD4DS
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCSPCD'
     C                   MOVEL     SCSPCD        HLSPCD            3
     C                   CLEAR                   ##KEY1
     C                   CALL      'F2335R'
     C                   PARM      '2'           ##CALL
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
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM                    ##RRN2
     C                   SETON                                        90
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   CLEAR                   SCDFC
     C                   CLEAR                   SCDIA1
     C                   CLEAR                   SCDIA2
     C                   CLEAR                   SCDIA3
     C                   CLEAR                   SCDIA4
     C                   CLEAR                   SCD1DS
     C                   CLEAR                   SCD2DS
     C                   CLEAR                   SCD3DS
     C                   CLEAR                   SCD4DS
     C                   CLEAR                   SCACCT
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
     C                   Z-ADD     EHDFC         SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDFC
     C*
     C                   MOVE(P)   EHDIA1        SCDIA1
     C                   MOVE(P)   SCDIA1        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCD1DS
     C*
     C                   MOVE(P)   EHDIA2        SCDIA2
     C                   MOVE(P)   SCDIA2        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCD2DS
     C*
     C                   MOVE(P)   EHDIA3        SCDIA3
     C                   MOVE(P)   SCDIA3        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCD3DS
     C*
     C                   MOVE(P)   EHDIA4        SCDIA4
     C                   MOVE(P)   SCDIA4        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCD4DS
     C*
     C                   MOVE(P)   EHACCT        SCACCT
     C*
     C     EHEDI         IFNE      *BLANKS
     C                   MOVE      EHEDI         SCEDI
     C                   ENDIF                                                  EHEDI NE *BLANKS
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
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     EOCO#         EHCO#
     C                   Z-ADD     EOSUB#        EHSUB#
     C                   Z-ADD     EOFND         EHFND
     C                   Z-ADD     EOFYR         EHFYR
     C                   Z-ADD     EOCASE        EHCASE
     C                   MOVE      EOSS#         EHSS#
     C                   Z-ADD     EOACDT        EHACDT
     C*
     C                   Z-ADD     SCDFC         SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EHDFC
     C*
     C                   MOVE(P)   SCDIA1        EHDIA1
     C                   MOVE(P)   SCDIA2        EHDIA2
     C                   MOVE(P)   SCDIA3        EHDIA3
     C                   MOVE(P)   SCDIA4        EHDIA4
     C                   MOVE(P)   SCACCT        EHACCT
     C                   MOVE      SCEDI         EHEDI
     C*** BEG ADD *** 02/21/00 ****************************************
     C                   MOVE      SCSPCD        EOSPCD
     C*** END ADD *** 02/21/00 ****************************************
     C*
     C*****                Z-ADDSCCHRG    EOCHRG
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
     C     *INKJ         CASEQ     *ON           KJDO                           F10=search
     C     *INKK         CASEQ     *ON           KKDO                           F11=EOB/Notes
     C                   ENDCS
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
     C     EHKY          CHAIN     PEH01                              99
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
     C* CALL 'ADJUDICATION CALL' PROGRAM IF IN UNPEND MODE
     C* THIS IS DONE BECAUSE OTHER BILLS MAY HAVE BEEN PROCOSSED
     C* FOR THIS CASE SINCE THIS BILL HAS BEEN ADJUDICATED
     C*
     C*** BEG DEL *** 11/12/99 ****************************************
     C********** EOBLST    IFEQ 'U'
     C********** *INKL     ANDNE*ON
     C********** ##RTCD    ANDNE'P'
     C**********           Z-ADD@@RRN2    ##RRN
     C**********           CALL 'FA713R'
     C**********           PARM ' '       ##KC
     C**********           PARM ' '       ##KL
     C**********           PARM           ##RRN
     C**********           ENDIF                           EOBLST = 'U'
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
     C*****                PARM           #1L2
     C                   MOVEL     #1L1          SCHD3
     C*****                MOVEL#1L2      SCHD4
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
     C*** BEG CHG *** 04/23/07 ****************************************
     C*****                MOVELEOAST     ##KEY
     C                   MOVEL     EDITST        ##KEY
     C*** END CHG *** 04/23/07 ****************************************
     C     STKY          CHAIN     PST01                              99
     C     *IN99         IFEQ      *OFF
     C     ST1A4         ANDEQ     'Y'
     C                   SETON                                        30
     C*** BEG CHG *** 04/23/07 ****************************************
     C*****                MOVE EOAST     KKAST
     C                   MOVE      EDITST        KKAST
     C*** END CHG *** 04/23/07 ****************************************
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
     C** DETERMINE IF NETWORK TITLE AND FIELD SHOULD BE DISPLAYED
     C**********
     C**********           MOVE 'FB'      KKKEY
     C********** SBKY      CHAINSFB01                99
     C********** *IN99     IFEQ *OFF
     C********** FBFL06    ANDEQ'Y'
     C**********           SETON                     40
     C**********           ELSE
     C**********           SETOF                     40
     C**********           ENDIF                           *IN99 IFEQ *OFF
     C*
     C** SET VARIOUS FIELDS
     C*
     C                   MOVE      'D'           KKLOST
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C*
     C* DISPLAY PROVIDER NETWORK IF NOT BLANKS
     C*
     C******     @@NWCD    IFNE *BLANKS
     C******               MOVEL@@NWCD    SCNWCD    P
     C     EONTWK        IFNE      *BLANKS
     C                   MOVEL(P)  EONTWK        SCNWCD
     C                   SETON                                        69
     C                   ELSE
     C                   SETOFF                                       69
     C                   ENDIF                                                  EONTWK
     C*
     C* SET INITIAL CURSOR POSITON TO DATE FIRST CONSULTED
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
     C                   MOVEL     Q1USER        EHUSER
     C                   MOVEL     Q1PGM         EHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EHTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EHDT
     C*
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C*
     C** UPDATE EOMB TRANSACTION FILE
     C*
     C     @@RRN2        CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  SCSPCD        EOSPCD
     C                   MOVEL(P)  SCDIA1        EODIA1
     C                   MOVEL(P)  SCDIA2        EODIA2
     C                   MOVEL(P)  SCDIA3        EODIA3
     C                   MOVEL(P)  SCDIA4        EODIA4
     C*** BEG ADD *** 09/10/99 ****************************************
     C                   MOVE      SCEDI         EOTAPE
     C*** END ADD *** 09/10/99 ****************************************
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     SRDTO         EODT
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C* If in ADD mode and @@RRN (FAEHP) is not zeros, then the user
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
     C                   Z-ADD     EOEOB#        EHEOB#
     C                   MOVEL     Q1USER        EHUSER
     C                   MOVEL     Q1USER        EHIUSR
     C                   MOVEL     Q1PGM         EHPGM
     C                   MOVEL     Q1PGM         EHIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EHTIME
     C                   Z-ADD     DSTIME        EHITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EHDT
     C                   MOVEL     SRDTO         EHIDT
     C*
     C                   WRITE     DBRCDF
     C     @@RRN2        CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C******               Z-ADDSCCHRG    EOCHRG
     C                   MOVEL(P)  SCSPCD        EOSPCD
     C                   MOVEL(P)  SCDIA1        EODIA1
     C                   MOVEL(P)  SCDIA2        EODIA2
     C                   MOVEL(P)  SCDIA3        EODIA3
     C                   MOVEL(P)  SCDIA4        EODIA4
     C*** BEG ADD *** 09/10/99 ****************************************
     C                   MOVE      SCEDI         EOTAPE
     C*** END ADD *** 09/10/99 ****************************************
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     SRDTO         EODT
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C                   FEOD      FAEHP
     C*
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
