     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE F0392R - Cancel/Pend selection
     H*****************************************************************
     H*                                                               *
     H*  CALLED BY: F0350R01                                          *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED:                                                     *
     H*                                                               *
     H*  06/23/98 C:61 PROJECT MC152                                  *
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
    h*                                                               *
    h*  05/08/24 C:344 Project: PHW115526                            *
     h*                 Revised the onclick property for the          *
     h*                 'Cancel' radio buttons to clear Pend Type     *
     h*                 and Description fields in display file F0392F.*
     h*                                                               *
     h*  04/30/19 C:079 Support: 090/75330                            *
     h*                 Modernize the screen                          *
     h*                                                               *
     h*  04/10/19 C:062 Project: 152WAB                               *
     h*                 Apply CSI mods for S1 Medical.                *
     H*                                                               *
     H*  12/05/11 C:190 PROJECT: 114ICM                               *
     H*                 CLEAR THE IMAGE ID, SO THAT BILLS CAN BE      *
     H*                 RE-ENTERED FROM WORK WITH INCOMING DOCS, IF   *
     H*                 CLTID = 114.                                  *
     H*                                                               *
     H*  11/30/00 C:62  PROJECT: MCMNT                                *
     H*                 -EDIT AGAINST INVALID PEND CODES.             *
     H*                                                               *
     H*  08/01/00 C:79 Support MC034/#23695                           *
     H*                Write 'P' to CAFLAG so line items wont show up *
     H*                in history                                     *
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*                                                               *
     H*  07/01/99 C:56 Project: MC202                                 *
     H*             1. If pending the bill, call Request Letter       *
     H*                program (F0750R) if Bill Status Type Code      *
     H*                entered has 'Y' in Pend Letter flag.           *
     F*
     H*             2. Send request letter to Bill Status History     *
     H*                program (F0395R01).                            *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H* SCR01                                                         *
     H*  71       'Cancel' must be 'Y' or *blank                      *
     H*  72       'Pend' must be 'Y' or *blank                        *
     H*  73       'Cancel' or 'Pend' must be 'Y'                      *
     H*  74       'Pend type' required if 'Pend' equal 'Y'            *
     H*  75 - 76  Position cursor                                     *
     H*  77       'Pend' required if 'Pend type' not blank            *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*****************************************************************
     F*
     F*
     F*
     FF0392F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFAEOP     UF A E             DISK
     F                                     RENAME(PEO01:DBRCDF)
     F*
     F*  Bill Status Table File
     FFTBSL     IF   E           K DISK
     FFTBSP     IF   E             DISK
     F                                     RENAME(PBS01:PBS02)
     F*
     F* WCMC Detail Charge File
     FFDCAL12   UF   E           K DISK
     F*
     F*
     F* WCMC Case Lockout File
     FFXLOP     UF   E           K DISK
     D***PY SCOPYMT01I
      /COPY SCOPYMT01D
     D*
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Cancel/Pend selectio-
     D                                     n')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D***** Beg Add *** 04/30/19 **************************************
     D inPtypeChg      s              1n   Inz(*on)
     D***** End Add *** 04/30/19 **************************************

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FAEOP
     C                   PARM                    @@CAN             1            Allow Cancel opt Y/b
     C                   PARM                    @@CHRG            9            HELD TOTAL CHARGE
     C                   PARM                    @@RTCD            1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C* EOMB Status Transaction
     C*
     C*****      ESKY      KLIST
     C*****                KFLD           KKCO#
     C*****                KFLD           KKSUB#
     C*****                KFLD           KKFND
     C*****                KFLD           KKFYR
     C*****                KFLD           KKCASE
     C*****                KFLD           KKEOB#
     C*
     C* Bill Status Table
     C*
     C     BSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKCODE
     C*
     C* key list for WMCM detail charge file
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR WCMC CASE LOCKOUT FILE
     C*
     C     LOKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CACO#         KKCO#
     C     *LIKE         DEFINE    CASUB#        KKSUB#
     C     *LIKE         DEFINE    CAFND         KKFND
     C     *LIKE         DEFINE    CAFYR         KKFYR
     C     *LIKE         DEFINE    CACASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    BSTYPE        KKTYPE
     C     *LIKE         DEFINE    BSCODE        KKCODE
     C     *LIKE         DEFINE    EOFEIN        KKFEIN
     C     *LIKE         DEFINE    EOMOD         KKMOD
     C*****      *LIKE     DEFN EOCHRG    TLCHRG
     C     *LIKE         DEFINE    CATOTA        TLTOTA
     C     *LIKE         DEFINE    CATOTD        TLTOTD
     C     *LIKE         DEFINE    EONDIS        TLNDIS
     C     *LIKE         DEFINE    EOOTHR        TLOTHR
     C     *LIKE         DEFINE    EOOTH2        TLOTH2
     C*
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C* Cancel     must be 'Y' or *blank
     C*
     C  N90SCCANC        IFNE      'Y'
     C     SCCANC        ANDNE     *BLANK
     C                   SETON                                        9071
     C                   ENDIF                                                  SCCANC
     C*
     C* Pend must be 'Y' or *blank
     C*
     C  N90SCPEND        IFNE      'Y'
     C     SCPEND        ANDNE     *BLANK
     C                   SETON                                        9072
     C                   ENDIF                                                  SCPEND
     C*
     C* Cancel     OR Pend must be 'Y'
     C*
     C  N90SCCANC        IFNE      'Y'
     C     SCPEND        ANDNE     'Y'
     C                   SETON                                        9073
     C                   ENDIF                                                  COMP  PEND
     C  N90SCCANC        IFEQ      'Y'
     C     SCPEND        ANDEQ     'Y'
     C                   SETON                                        9073
     C                   ENDIF                                                  COMP  PEND
     C*
     C* Pend type required if Pend = 'Y'
     C*
     C  N90SCPEND        IFEQ      'Y'
     C     SCPTYP        ANDEQ     *BLANKS
     C                   SETON                                        9074
     C                   ENDIF                                                  SCPEND SCPTYP
     C*
     C* 'Pend' must equal 'Y' if 'Pend Type' entered
     C*
     C  N90SCPTYP        IFNE      *BLANKS
     C     SCPEND        ANDEQ     *BLANKS
     C                   SETON                                        9077
     C                   ENDIF                                                  SCPTYP SCPEND
     C*
     C* Pend type must be valid if entered
     C*
     C  N90SCPTYP        IFNE      *BLANKS
     C                   MOVEL(P)  'P'           KKTYPE                         PEND
     C                   MOVEL(P)  SCPTYP        KKCODE
     C     BSKY          CHAIN     PBS01                              60
     C*** BEGIN CHG *** 11/30/00 **************************************
     C***60*****           SETON                     9078
     C     *IN60         IFEQ      *ON
     C     BSSTAT        OREQ      'I'
     C                   SETON                                        9078
     C                   ENDIF
     C*** END   CHG *** 11/30/00 **************************************
     C                   ENDIF                                                  SCPTYP VALIDP
     C*
     C* If reason = *blank and Pend = 'Y' then load reason with
     C*  Pend Type Description
     C*
     C  N90SCREAS        IFEQ      *BLANKS
     C     SCPEND        ANDEQ     'Y'
     C                   MOVEL(P)  'P'           KKTYPE                         PEND
     C                   MOVEL(P)  SCPTYP        KKCODE
     C     BSKY          CHAIN     PBS01                              60
     C  N60              MOVEL(P)  BSDESC        SCREAS
     C   60              MOVEL(P)  *BLANKS       SCREAS
     C                   ENDIF                                                  SCREAS SCPEND
     C*
     C*** BEG ADD *** 07/01/99 ****************************************
     C*
     C* CALL REQUEST LETTER PROGRAM IF PENDING BILL AND PEND LETTER
     C* FLAG IS 'Y'.
     C*
       //*** Beg Add *** 04/30/19 ******************************************
      /Free
                      If *in90 = *off;
      /End-free
       //*** End Add *** 04/30/19 ******************************************
     C     SCPEND        IFEQ      'Y'
     C     BSLTR         ANDEQ     'Y'
     C                   MOVEL(P)  OPTNM         ##STR
     C                   CALL      'SRCENTER'
     C                   PARM                    ##STR            80
     C                   PARM      40            ##LEN             3 0
     C                   MOVEL(P)  ##STR         ##PRDS           40
     C                   MOVE      EOSUB#        ##SUB#            5
     C                   MOVE      EOFND         #1FND             3
     C                   MOVE      EOFYR         ##FYR             3
     C                   MOVE      EOCASE        ##CASE            7
     C                   MOVE      EOFEIN        ##FEIN            9
     C                   MOVE      EOMOD         ##MOD             3
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##FSDT
     C                   Z-ADD     EOTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##TSDT
     C                   MOVE      EOCHRG        ##TTLC
     C                   MOVE      *ZEROS        ##LTR#
     C*
     C     EOBTYP        IFEQ      'H'
     C                   MOVE      '4'           ##PRTY
     C                   ELSE
     C                   MOVE      '1'           ##PRTY
     C                   ENDIF                                                  EOBTYP = 'H'
     C*
     C     EOCSCD        IFEQ      '320'
     C                   MOVE      'N'           ##PRNT
     C                   ELSE
     C                   MOVE      *BLANKS       ##PRNT                         EOCSCD = '320'
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       ##RTCD            1
     C                   Z-ADD     EOEOB#        #1EOB#
     C                   Z-ADD     EORCN         ##RCN
     C                   CALL      'F0750R'
     C                   PARM                    ##PRDS
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##SUB#
     C                   PARM                    #1FND
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C                   PARM                    ##FEIN
     C                   PARM                    ##MOD
     C                   PARM                    ##FSDT            8
     C                   PARM                    ##TSDT            8
     C                   PARM                    ##TTLC            9
     C                   PARM                    ##LTR#            9
     C                   PARM                    ##PRTY            1
     C                   PARM                    ##PRNT            1
     C                   PARM                    ##RTCD
     C                   PARM                    #1EOB#            9 0
     C                   PARM                    ##RCN             3 0
     C*
     C     ##RTCD        IFEQ      'N'
     C                   SETON                                        90
     C                   ENDIF                                                  ##RTCD = 'N'
     C*
     C                   ENDIF                                                  SCPEND = 'Y'
     C*** END ADD *** 07/01/99 ****************************************
       //*** Beg Add *** 04/30/19 ******************************************
      /Free
                      Endif;
      /End-free
       //*** End Add *** 04/30/19 ******************************************
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KJDO - F10 processing
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   MOVE      '1'           *IN90
     C*
     C                   SELECT
     C*
     C*  Search Pend types
     C*** Beg Del *** 04/30/19 *********************************************
     C****
     C**** HFLD          WHENEQ    'SCPTYP'
     C****
     C****               CALL      'F2435R'
     C****               PARM      '2'           ##CALL            1
     C****               PARM      *ZERO         ##KC              1
     C****               PARM      *ZERO         ##KL              1
     C****               PARM      *ZERO         ##RRN             9 0
     C****               PARM      'P'           ##TYPE            1
     C****
     C**** ##RRN         IFEQ      *ZEROS
     C****               SETON                                        75
     C****               ELSE
     C****
     C**** ##RRN         CHAIN     PBS02                              60
     C**N60              MOVEL(P)  BSCODE        SCPTYP
     C**N60              MOVEL(P)  BSDESC        SCPTDS
     C****               SETON                                        76
     C****               ENDIF
     C****
     C*** End Del *** 04/30/19 *********************************************
     C                   ENDSL
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
       //*** Beg Add *** 04/30/19 *************************************
       // Load the choices for document filter.
      /Free

       Clear scBSChoic;
       Clear scBSChVal;

       Setll (EOco#:'P':' ') pbs01;
       Reade (EOco#:'P') pbs01;
       Dow not %eof(ftbsl);
          If BSstat = 'A';
             If scBSChoic = *blanks;
                scBSChoic = ' ,' + %trim(BScode) + ' - ' + %trim(BSdesc);
                scBSChVal = ' ,' + %trim(BScode);
             Else;
                scBSChoic = %trim(scBSChoic) + ',' + %trim(BScode) +
                             ' - ' + %Trim(BSdesc);
                scBSChVal = %trim(scBSChVal) + ',' + %trim(BScode);
             Endif;
          Endif;
          Reade (EOco#:'P') pbs01;
       Enddo;
      /End-Free
       //*** End Add *** 04/30/19 *************************************
     C*
     C*  Default Cancel and Pend Flags
     C*
     C  N30              MOVEL(P)  'Y'           SCCANC
     C*
     C*** Beg Del *** 04/30/19 *********************************************
     C*****
     C*****SCPTYP        IFNE      *BLANKS
     C*****              MOVEL(P)  'P'           KKTYPE                         PEND
     C*****              MOVEL(P)  SCPTYP        KKCODE
     C*****BSKY          CHAIN     PBS01                              60
     C**N60              MOVEL(P)  BSDESC        SCPTDS
     C***60              MOVEL(P)  *BLANKS       SCPTDS
     C*****              ENDIF                                                  SCPTYP
     C*** End Del *** 04/30/19 *********************************************
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C* If Pend = 'Y'
     C*
     C     SCPEND        IFEQ      'Y'
     C                   MOVE      'P'           EOBLST
     C                   ENDIF                                                  SCPEND = Y
     C*
     C     SCCANC        IFEQ      'Y'
     C                   MOVE      'X'           EOBLST
     C*** BEG ADD *** 12/05/11 ****************************************
     C     XXCLT#        IFEQ      '114'
     C*** Begin Add *** 04/10/19 **************************************
     C     XXCLT#        OREQ      '152'
     C*** End   Add *** 04/10/19 **************************************
     C                   CLEAR                   EOIDOC
     C                   ENDIF
     C*** BEG ADD *** 12/05/11 ****************************************
     C                   ENDIF                                                  SCCANC = Y
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL(P)  Q1USER        EOUSER
     C                   MOVEL(P)  Q1PGM         EOPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   MOVEL     DSTIME        EOTIME
     C*
     C                   MOVEL(P)  SCPTYP        ##STCD            3
     C                   MOVEL(P)  SCREAS        ##REAS           60
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C*  Process F10 search
     C*
     C     *INKJ         CASEQ     '1'           KJDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C     SCPEND        IFEQ      'Y'
     C                   MOVE      'P'           @@RTCD
     C                   ENDIF
     C*
     C                   SETON                                        LR
     C                   MOVE      '1'           OUTFLG
     C*
     C* REMOVE RECORD FROM LOCKOUT FILE
     C*
     C     @@MODE        IFEQ      'A'
     C     @@MODE        OREQ      'C'
     C     LOKY          CHAIN     PLO01                              99
     C     *IN99         IFEQ      *OFF
     C                   DELETE    PLO01
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  @@MODE = 'A'
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C* Set Change Mode
     C*
     C                   MOVEL(P)  'C'           @@MODE
     C*
     C* Get FAEOP record
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C*
     C* Load key fields
     C*
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C                   Z-ADD     EOFEIN        KKFEIN
     C                   Z-ADD     EOMOD         KKMOD
     C*
     C*
     C* Determine if Cancel Bill option is allowed.
     C*
     C*****      @@CAN     IFNE 'A'
     C*****                MOVE 'Y'       SCPEND
     C*****                SETON                     30
     C*****                ELSE
     C*****                SETOF                     30
     C*****                ENDIF                           @@CAN = 'A'
     C*
     C     @@CAN         IFEQ      'Y'
     C                   SETOFF                                       30
     C                   ELSE
     C                   MOVE      'Y'           SCPEND
     C                   SETON                                        30
     C                   ENDIF
     C*** BEG ADD *** 12/05/11 ****************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*** END ADD *** 12/05/11 ****************************************
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C*
     C* CALCULATE TOTAL AMOUNTS FOR THE BILL
     C*
     C*****                Z-ADD0         TLCHRG
     C**********           Z-ADD0         TLTOTA
     C**********           Z-ADD0         TLTOTD
     C**********           Z-ADD0         TLNDIS
     C**********           Z-ADD0         TLOTHR
     C**********           Z-ADD0         TLOTH2
     C********** CAKY      CHAINPCA01               N99
     C********** *IN99     DOWEQ*OFF
     C**********           ADD  CACHRG    TLCHRG
     C**********           ADD  CATOTA    TLTOTA
     C**********           ADD  CATOTD    TLTOTD
     C**********           ADD  CANDIS    TLNDIS
     C**********           ADD  CAOTHR    TLOTHR
     C**********           ADD  CAOTH2    TLOTH2
     C********** CAKY      READEPCA01               N    99
     C**********           ENDDO
     C**********
     C**********           Z-ADDTLCHRG    EOCHRG
     C**********           Z-ADDTLTOTA    EOPAID
     C**********           Z-ADDTLTOTD    EOMRA
     C**********           Z-ADDTLNDIS    EONDIS
     C**********           Z-ADDTLOTHR    EOOTHR
     C**********           Z-ADDTLOTH2    EOOTH2
     C*
     C*
     C*** IF USER DECIDES TO PEND THE BILL ON A CHANGE AND THE TOTAL
     C*** CHARGE WAS CHANGED IN PROVIDER BILL INFORMATION (F0350R01),
     C*** THEN WE MUST RESTORE THE PREVIOUS TOTAL CHARGE (PARM @@CHRG)
     C*** SO IT WILL X-FOOT WITH THE DETAIL CHARGES.
     C*
     C                   Z-ADD     0             HLCHRG            9 2
     C                   MOVE      @@CHRG        HLCHRG
     C     @@MODE        IFEQ      'C'
     C     SCPEND        ANDEQ     'Y'
     C     EOCHRG        ANDNE     HLCHRG
     C                   Z-ADD     HLCHRG        EOCHRG
     C                   ENDIF                                                  @@MODE = 'C'
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* IF BILL STATUS IS PENDED, CANCELLED, OR DELETED WRITE A 'P'
     C* IN CAFLAG.
     C*
     C     EOBLST        IFEQ      'P'
     C*** BEG ADD *** 08/01/00 ****************************************
     C     EOBLST        OREQ      'X'
     C     EOBLST        OREQ      'D'
     C*** END ADD *** 08/01/00 ****************************************
     C     CAKY          CHAIN     PCA01                              99
     C     *IN99         DOWEQ     *OFF
     C                   MOVE      'P'           CAFLAG
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   UPDATE    PCA01
     C     CAKY          READE     PCA01                                  99
     C                   ENDDO
     C                   ENDIF                                                  EOBLST = 'P'
     C*
     C* CALL F0395R01 TO WRITE BILL STATUS HISTORY RECORD
     C*
     C                   Z-ADD     0             ##RRN             9 0
     C                   Z-ADD     @@RRN         ##RRN2            9 0          faeop
     C                   MOVE      EOBLST        ##BLST            1
     C                   MOVEL(P)  SCPTYP        ##STCD
     C                   MOVEL(P)  SCREAS        ##REAS
     C                   MOVE      *BLANKS       ##PGM             8
     C*** BEG ADD *** 07/01/99 ****************************************
     C                   MOVE      ##LTR#        #1LTR#            9 0
     C*** END ADD *** 07/01/99 ****************************************
     C                   CALL      'F0395R01'
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      'H'           ##MODE            1
     C                   PARM                    ##RRN
     C                   PARM                    ##RRN2
     C                   PARM                    ##BLST
     C                   PARM                    ##STCD
     C                   PARM                    ##REAS
     C                   PARM                    ##PGM
     C*** BEG ADD *** 07/01/99 ****************************************
     C                   PARM                    #1LTR#
     C*** END ADD *** 07/01/99 ****************************************
     C                   ENDSR
     C*
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FAEOP
     C                   ENDSR
