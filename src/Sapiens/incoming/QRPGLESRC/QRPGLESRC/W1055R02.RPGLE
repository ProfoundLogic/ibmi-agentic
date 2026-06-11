     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      *===============================================================*
     H/TITLE W1056R02 - MAINTAIN MIDTERM ENDORSEMENT FORM TABLE RECORDS
      *===============================================================*
      * Program Description:                                          *
      *   This program will allow the user to change or add an        *
      *   Endorsement.                                                *
      *                                                               *
      * Created   : 08/17/99                                          *
      * Programmer: C:113                                             *
      * Project   : WC904                                             *
      *===============================================================*
      * REVISIONS:                                                    *
      *                                                               *
      * 08/20/18 C:291 Project: 133WQP                                *
      *                Add Endorsement Type                           *
      *                                                               *
      * 01/31/18 C:62  Project: 150WCO                                *
      *                -Override Form #                               *
      *                -National/State Form #                         *
      *                -Exclude from WCPOLS flag                      *
      *                                                               *
      * 01/09/18 C:62  Project: 133WQH                                *
      *                Add 'Adoption Date'                            *
      *                                                               *
      * 11/08/17 C:65  Project: 150WCC                                *
      *                Add 'Generate from PDF' flag.                  *
      *                                                               *
      * 11/03/17 C:136 Project: 150WCC                                *
      *                Add 'Exclude from Schedule' flag               *
      *                                                               *
      * 10/06/17 C:65  Project: 150WAP                                *
      *                Add PEO relationship dropdown to allow setup   *
      *                to determine how endorsement should attach     *
      *                for PEO groups.                                *
      *                                                               *
      * 09/14/17 C:291 Project: 150WBM                                *
      *                Allow Client Template path for all the documents
      *                                                               *
      * 05/23/17 C:180 Support 46953                                  *
      *                Fix Endorsement Description logic causing      *
      *                unnecessary edit errors.                       *
      *                                                               *
      * 05/16/17 C:65  Project: SY371                                 *
      *                Add path field for client endorsement templates*
      *                                                               *
      * 03/01/17 C:062 Project: WC2481                                *
      *                Bypass document/folder edit if Scriptura and   *
      *                Generarte Spool is N                           *
      *                                                               *
      * 02/28/17 C:180 Project: WC2481                                *
      *                Add new form position field ENDPOS.            *
      *                                                               *
      * 11/10/16 C:136 Project: SY371                                 *
      *                Use extended desscription field                *
      *                                                               *
      * 06/16/14 C:282 Redmine 4329                                   *
      *                Condition added to pass through the 'Mid-Year' *
      *                Form.                                          *
      *                                                               *
      * 11/05/08 C:190 PROJECT WC1700                                 *
      *                Allow for both an 'Attach program' and         *
      *                'Additional info program' to be entered        *
      *                for the 'Alternate Employer Endorsement'.      *
      *                                                               *
      * 09/22/08 C:61  PROJECT WC1744 PRISS 22                        *
      *                Entry of Replacement Form is no longer
      *                required with entry of replacement date.
      *
      * 04/04/08 C:33  Seq#: WA136/62706
      *                Update edits for folder, pgm1, pgm2 & pgm3
      *                 based on flags.
      *
      * 03/19/08 C:33  Seq#: WA136/62706
      *                Split field edtiting.
      *
      * 03/19/08 C:33  Seq#: WA136/62706
      *                Split field edtiting.
      *
      *  2/23/07 C:61  support WA135 57328
      *                Update screen file to display edits correctly.
      *
      * 12/02/06 C:61  support wa099 41166
      *              - ADD mode; SCR01 Document must exist on system
      *                in folder entered.
      *              - Correct "Replacement date" edit.
      *              - "Folder" edit regardless of whether printed.
      *
      * 12/01/06 C:61  support wa099 43901
      *                Reinstate folder validation edit.
      *
      * 09/16/04 C:61  PROJECT 132WAC
      *                Remove edit to check for valid folder until
      *                SRCHKDLO can be modified
     H*                                                               *
     H* 01/02/03 C:61  PROJECT CM027                                  *
     H*              - Correct Replacement Form entry edit SCR02.     *
     H*                Screen file change only.                       *
     H*                                                               *
     H* 12/06/02 C:61  PROJECT CM027                                  *
     H*              - Add search on state field.                     *
     H*              - Add edit for pgm1 and pgm2                     *
     H*
     H*  11/10/00 C:90  SUPPORT: SEQ# 25017
     H*                 CHANGED INDICATOR FOR SINGLE/MULT. FORM ERROR
     H*                 CHECK.
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     *===============================================================*
     FW1055F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
     ** Physical file definition (for maintaining record)
     FWTENP     UF A E             DISK
     F                                     RENAME(PEN01:DBRCDF)
     FWTENL     IF   E           K DISK
      *
     ** System Table File
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
      ***** BEG ADD ******* 12/06/02 **********************************
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PST02)
      ***** END ADD ******* 12/06/02 **********************************
      *
     ** Group Master File
     FWMFNL     IF   E           K DISK
      *
    D***PY SCOPYMT01I
      *
     *===============================================================*
     *  /COPY required constants
     *===============================================================*
      /COPY SCOPYMT01D
     d*** Begin Add *** 03/01/17 *********************************************
     D isScriptUsed    s              1A   inz('N')
     D isUseSpoolOut   s               n   inz(*Off)
     D isUseXML        s               n   inz(*Off)
     D skipDocEdit     s               n   inz(*Off)
     D procType        s            100
     D/copy sPrXML
     d*** End   Add *** 03/01/17 *********************************************
     d*** begin add ***  05/16/17  ******************************************
     d strSlash        s              1
     d strSlash2       s              1
     d*** end add *****  05/16/17  ******************************************
     D OPTNM           C                   CONST('Endorsement Forms')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(2)
     *===============================================================*
     *  PLIST definitions
     *===============================================================*

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@FND             3 0
      *
     ** Company file/fund (group) retrieve program parameter list
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
     ** SRCHKDLO doc/flr checking API program parameter list
     C     DLOPL         PLIST
     C                   PARM      SCFLDR        ##DLO            12
     C                   PARM      *BLANKS       ##FLR            32
     C                   PARM      '*FLR'        ##DTYP            4
     C                   PARM      '0'           ##ERR             1
      *
     ** SRCHKOBJ object checking API program parameter list
     C     OBJPL         PLIST
     C                   PARM                    ##OBJ            10
     C                   PARM      '*PGM'        ##OBJT            7
     C                   PARM      '0'           ##ERR
     *===============================================================*
     *  KLIST definitions
     *===============================================================*
     C     ENKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##FORM
      *
     ** System Table file
     C     STKY          KLIST
     C                   KFLD                    XXKEY
      *
     ** Group Master file
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     *===============================================================*
     *  *LIKE DEFN statements
     *===============================================================*
     C     *LIKE         DEFINE    ENFND         ##FND
     C     *LIKE         DEFINE    ENFORM        ##FORM
     C     *LIKE         DEFINE    STKEY         XXKEY
     C*** Begin Add *** 01/09/18 ****************************
     C     *LIKE         DEFINE    ENEDDT        XXEDDT
     C     *LIKE         DEFINE    ENADTE        XXADTE
     C*** End   Add *** 01/09/18 ****************************
      *
    C/COPY SCOPYMT01C
      *
     *===============================================================*
     * CHGSTS - SUBROUTINE FOR CHANGING STATUS
     *===============================================================*
     C     CHGSTS        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C     *IN99         IFEQ      *OFF
      *
     C     ENSTAT        IFEQ      'I'
     C                   MOVE      'A'           ENSTAT
     C                   ELSE
     C                   MOVE      'I'           ENSTAT
     C                   ENDIF                                                  ENSTAT
      *
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENDT
      *
     C                   MOVEL     DSTIME        ENTIME
      *
     C                   UPDATE    DBRCDF
      *
     C                   ENDIF                                                  *IN99
      *
     C                   ENDSR                                                  CHGSTS
     *===============================================================*
     * DLTPRE - Pre-processing before delete occurs
     *===============================================================*
     C     DLTPRE        BEGSR
     C                   ENDSR
     *===============================================================*
     * DSPLYR - Processing after displaying information
     *===============================================================*
     C     DSPLYR        BEGSR
     C                   ENDSR
     *===============================================================*
     * ERRCK1 - Error check for screen 1
     *===============================================================*
     C     ERRCK1        BEGSR
      *
     ** Group field cannot be less than or equal to zero
     C     *IN40         IFEQ      *ON                                          ADD MODE
     C     *IN41         OREQ      *ON                                          COPY MODE
     C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C     SCFND         ANDLE     *ZERO
     C                   SETON                                        52  90
     C                   ENDIF                                                  SCFND
     C                   ENDIF                                                  SCFND
      *
     ** Group field must be valid
     C                   MOVEL     SCFND         ##FND
      *
     C     *IN40         IFEQ      *ON                                          ADD MODE
     C     *IN41         OREQ      *ON                                          COPY MODE
     C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C     FNKY          CHAIN     PFN01                              99
     C   99              SETON                                        53  90
     C  N99              MOVEL     FNNAME        SCFNNM
     C                   ENDIF                                                  SCFND
     C                   ENDIF                                                  SCFND
      *
     ** Form field cannot be blank
     C     *IN40         IFEQ      *ON                                          ADD MODE
     C     *IN41         OREQ      *ON                                          COPY MODE
     C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C     SCFORM        ANDEQ     *BLANK
     C*** Begin Add *** 03/01/17 ***********************************************
     C     skipDocEdit   AndEQ     *Off
     C*** End   Add *** 03/01/17 ***********************************************
     C                   SETON                                        72  90
     C                   ENDIF                                                  SCFORM
     C                   ENDIF                                                  SCFORM
      *
     ** Check to see if form has been used previously
     C                   MOVEL     SCFORM        ##FORM
     C                   Z-ADD     SCFND         ##FND
      *
     C     *IN40         IFEQ      *ON                                          ADD MODE
     C     *IN41         OREQ      *ON                                          COPY MODE
     C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C     ENKY          SETLL     PEN01                                  99
     C   99              SETON                                        73  90
     C                   ENDIF
     C                   ENDIF
      *
     ** Description cannot be blank
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        74  90
     C                   ENDIF                                                  SCDESC
      *
     ** Edition date must be valid if entered
     C     *IN90         IFEQ      *OFF
      *
     C                   Z-ADD     SCEDDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C     SRERR         IFEQ      *ON
     C                   SETON                                        86  90
     C                   ENDIF                                                  SRERR=*ON
      *
     C                   ENDIF                                                  *IN90 = *OFF
     C*** Begin Add *** 01/09/18 *******************************************
     C                   Eval      mgAdte = ' '
     C                   Eval      erAdte = *Off
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
     C                   Z-ADD     scEdDt        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         xxEdDt

     C                   Z-ADD     scAdte        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         xxAdte

     C                   If        xxAdte < xxEdDt
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
     C*** begin add ***  05/16/17  ******************************************
      /free
          Clear msPathC;
          Clear erPathC;

          If *in90 = *off and scPathC <> ' ';

          // Document path must begin and end with a '/' character.
             strSlash  = %subst(scPathC:1:1);
             strSlash2 = %subst(scPathC:%len(%trim(scPathC)):1);

             If strSlash <> '/' or strSlash2 <> '/';
                msPathC = 'Document path must begin and end with a ''/''.';
                erPathC = *on;
                *in90 = *on;
             Endif;

          Endif;

      /end-free
     C*** end add *****  05/16/17  ******************************************
      *
     ** Manual/Auto generation flag must be 'M' or 'A'
     C  N90SCMNAU        IFNE      'A'
     C     SCMNAU        ANDNE     'M'
     C                   SETON                                        75  90
     C                   ENDIF                                                  SCSRFL <> N
      *
     ** Print form must be 'Y' or 'N'
     C  N90SCPRNT        IFNE      'Y'
     C     SCPRNT        ANDNE     'N'
     C                   SETON                                        51  90
     C                   ENDIF                                                  SCSRFL <> N
     C***  Begin Add  ***  02/28/17  **************************************
     ** Form Position must be P,B,A
      *
      /Free
       *in54 = *Off;
       If scEndPos <> *blanks and scEndPos <> 'D' and
          scEndPos <> 'B' and scEndPos <> 'A';
        *In54 = *on;
        *IN90=  *ON;
       Endif;
      /End-Free

     C***  End   Add  ***  02/28/17  **************************************
     C***  Begin Add  ***  11/05/08  **************************************
     C                   MOVE      *OFF          WC301             1
     C                   MOVEL(P)  SCFORM        #STMP1            8
      *
     C     #STMP1        IFEQ      'WC000301'
     C                   MOVE      *ON           WC301
     C                   ENDIF
     C***  End   Add  ***  11/05/08  **************************************
     C***  Begin Add  ***  04/04/08  **************************************
     C                   SELECT
     C*
     ** If manual form and printing,
     C     SCMNAU        WHENEQ    'M'
     C     SCPRNT        ANDEQ     'Y'
     C                   SELECT
     **   Folder must not be blank
     C     SCFLDR        WHENEQ    *BLANK
     C*** Begin Add *** 03/01/17 ***********************************************
     C     skipDocEdit   AndEQ     *Off
     C*** End   Add *** 03/01/17 ***********************************************
     C                   SETON                                        77  90
     **   Program 1 must be blank
     C     SCPGM1        WHENNE    *BLANK
     C***  Begin Add  ***  11/05/08  **************************************
     C     WC301         ANDNE     *ON
     C***  End   Add  ***  11/05/08  **************************************
     C                   SETON                                        36  90
     **   Program 2 must not be blank
     C     SCPGM2        WHENEQ    *BLANK
     C                   SETON                                        33  90
     C                   ENDSL
     **   Validate document exists
     C*** Begin Add *** 03/01/17 ***********************************************
     C                   If        skipDocEdit = *Off
     C*** End   Add *** 03/01/17 ***********************************************
     C  N90              EXSR      VALDOC
     C*** Begin Add *** 03/01/17 ***********************************************
     C                   EndIf
     C*** End   Add *** 03/01/17 ***********************************************
     C*
     ** If auto form and printing,
     C     SCMNAU        WHENEQ    'A'
     C     SCPRNT        ANDEQ     'Y'
     C                   SELECT
     **   Folder must not be blank
     C     SCFLDR        WHENEQ    *BLANK
     C*** Begin Add *** 03/01/17 ***********************************************
     C     skipDocEdit   AndEQ     *Off
     C*** End   Add *** 03/01/17 ***********************************************
     C                   SETON                                        77  90
     **   Program 1 must not be blank
     C     SCPGM1        WHENEQ    *BLANK
     C                   SETON                                        79  90
     **   Program 2 must not be blank
     C     SCPGM2        WHENEQ    *BLANK
     C                   SETON                                        33  90
     **   Program 3 must be blank
     C     SCPGM3        WHENNE    *BLANK
     C***  Begin Add  ***  11/05/08  **************************************
     C     WC301         ANDNE     *ON
     C***  End   Add  ***  11/05/08  **************************************
     C                   SETON                                        38  90
     C                   ENDSL
     **   Validate document exists
     C*** Begin Add *** 03/01/17 ***********************************************
     C                   If        skipDocEdit = *Off
     C*** End   Add *** 03/01/17 ***********************************************
     C  N90              EXSR      VALDOC
     C*** Begin Add *** 03/01/17 ***********************************************
     C                   EndIf
     C*** End   Add *** 03/01/17 ***********************************************
     C*
     ** If manual form and not printing,
     C     SCMNAU        WHENEQ    'M'
     C     SCPRNT        ANDEQ     'N'
     C                   SELECT
     **   Program 1 must be blank
     C     SCPGM1        WHENNE    *BLANK
     C***  Begin Add  ***  11/05/08  **************************************
     C     WC301         ANDNE     *ON
     C***  End   Add  ***  11/05/08  **************************************
     C                   SETON                                        36  90
     **   Program 2 must be blank
     C     SCPGM2        WHENNE    *BLANK
     C                   SETON                                        37  90
     C                   ENDSL
     C*
     ** If auto form and not printing,
     C     SCMNAU        WHENEQ    'A'
     C     SCPRNT        ANDEQ     'N'
     C                   SELECT
     **   Program 1 must not be blank
     C     SCPGM1        WHENEQ    *BLANK
     C                   SETON                                        79  90
     **   Program 2 must be blank
     C     SCPGM2        WHENNE    *BLANK
     C                   SETON                                        37  90
     **   Program 3 must be blank
     C     SCPGM3        WHENNE    *BLANK
     C***  Begin Add  ***  11/05/08  **************************************
     C     WC301         ANDNE     *ON
     C***  End   Add  ***  11/05/08  **************************************
     C                   SETON                                        38  90
     C                   ENDSL
     C*
     C                   ENDSL
     C***  End   Add  ***  04/04/08  **************************************
     C***  Begin Del  ***  04/04/08  **************************************
      **********
     ***If*manual form and not printing, folder and programs must be blank
     C********** *IN90     IFEQ *OFF
     C********** SCMNAU    ANDNE'A'
     C********** SCPRNT    ANDNE'Y'
      **********
     C********** SCFLDR    IFNE *BLANK
     C*****Begin Del  ***  03/19/08  **************************************
     C********** SCPGM1    ORNE *BLANK
     C********** SCPGM2    ORNE *BLANK
     C********** SCPGM3    ORNE *BLANK
     C*****End** Del  ***  03/19/08  **************************************
     C**********           SETON                     76  90
     C**********           ENDIF                           SCFLDR <> ' '
     C**********
     C*****Begin Add  ***  03/19/08  **************************************
     C**N90***** SCPGM1    IFNE *BLANK
     C**********           SETON                     36  90
     C**********           ENDIF                           SCPGM1 <> ' '
     C**********
     C**N90***** SCPGM2    IFNE *BLANK
     C**********           SETON                     37  90
     C**********           ENDIF                           SCPGM2 <> ' '
     C**********
     C**N90***** SCPGM3    IFNE *BLANK
     C**********           SETON                     38  90
     C**********           ENDIF                           SCPGM3 <> ' '
     C*****End** Add  ***  03/19/08  **************************************
      **********
     C**********           ENDIF                           *IN90=*OFF, SCMNAU<>A
      **********
     ***If*printable form, edit folder
     C********** *IN90     IFEQ *OFF
      ******BEG*DEL ******* 12/02/06 **********************************
     C********** SCPRNT    ANDEQ'Y'
      ******END*DEL ******* 12/02/06 **********************************
      **********
     ***Folder*name may not be blank, and must be valid
     C********** SCFLDR    IFEQ *BLANK
     C**********           SETON                     77  90
      **********
     C**********           ELSE
      **********
      ******BEG*DEL ***** 09/16/04 ************************************
     C**********           CALL 'SRCHKDLO'DLOPL
     C********** ##ERR     IFEQ *ON
     C**********           SETON                     78  90
     C**********           ENDIF                           ##ERR=*ON
      ******END*DEL ***** 09/16/04 ************************************
      ******BEG*ADD ******* 12/01/06 **********************************
      **********
     C**********           MOVELSCFLDR    #1FOLD    P
      **********
     C**********           CALL 'SVALFLR'
     C**********           PARM           #1FOLD 80
     C**********           PARM *BLANK    #1RTCD  1
      **********
      ***Folder*must exist and user must have authority to folder
      **********
     C********** #1RTCD    IFEQ '1'
     C********** #1RTCD    OREQ '2'
     C**********           SETON                     78  90
     C**********           ENDIF                           #1RTCD
      **********
      ******END*ADD ******* 12/01/06 **********************************
      **********
     C**********           ENDIF                           SCFLDR <> ' '
      **********
     C**********           ENDIF                           SCPRNT = 'Y'
      ******BEG*ADD ******* 12/02/06 **********************************
      **********
     ***If*ADD*mode, Document must exist in specified folder
     C**N90***** *IN40     IFEQ *ON                        ADD MODE
     C********** *IN41     OREQ *ON                        COPY MODE
     C**********           MOVELSCFLDR    #2FOLD    P
      **********
     C**********           MOVELSCFORM    #2TMP1  8 P
     C**********           MOVE SCFORM    #2TMP2  7 P
     C**********           MOVEL#2TMP1    #2DCID    P
      **********
     C********** #2TMP2    IFNE *BLANK
     C**********           CAT  '.':0     #2DCID    P
     C**********           CAT  #2TMP2:0  #2DCID    P
     C**********           ENDIF
      **********
     C**********           CALL 'SVALDOC'
     C**********           PARM           #2DCID 12
     C**********           PARM           #2FOLD 80
     C**********           PARM *BLANK    #2RTCD  1
      **********
     C********** #2RTCD    IFEQ '1'
     C**********           SETON                     90  88
     C**********           ENDIF                           QQRTCD IFEQ '1'
      **********
     C**********           ENDIF                           ADD OR COPY
      ******END*ADD ******* 12/02/06 **********************************
      **********
     ***If*automatic form or printable, edit programs
     C********** SCMNAU    IFEQ 'A'
     C********** SCPRNT    OREQ 'Y'
      **********
     ***Program 1 must not be blank if auto attach
     C********** *IN90     IFEQ *OFF
     C********** SCMNAU    ANDEQ'A'
     C********** SCPGM1    ANDEQ*BLANK
     C**********           SETON                     79  90
     C**********           ENDIF                           IN90, SCPGM1 = ' '
      ******BEG*ADD ******* 12/06/02 **********************************
      **********
     ***Program 1 must be blank if manual attach
     C********** *IN90     IFEQ *OFF
     C********** SCMNAU    ANDEQ'M'
     C********** SCPGM1    ANDNE*BLANK
     C**********           SETON                     32  90
     C**********           ENDIF
      ******END*ADD ******* 12/06/02 **********************************
      **********
     ***Program 2 must not be blank if form is printable
     C********** *IN90     IFEQ *OFF
     C********** SCPRNT    ANDEQ'Y'
     C********** SCPGM2    ANDEQ*BLANK
     C**********           SETON                     80  90
     C**********           ENDIF                           IN90/SCPGM2
     C***  End   Del  ***  04/04/08  **************************************
     C*
     C***  Begin Add  ***  04/04/08  **************************************
     ** Folder name must be valid, if entered
     C  N90SCFLDR        IFNE      *BLANK
     C*** Begin Add *** 03/01/17 ***********************************************
     C     skipDocEdit   AndEQ     *Off
     C*** End   Add *** 03/01/17 ***********************************************
     C                   EXSR      VALFLR
     C                   ENDIF
     C***  End   Add  ***  04/04/08  **************************************
      *
     ** Program 1, if entered, must be valid
     C     *IN90         IFEQ      *OFF
     C     SCPGM1        ANDNE     *BLANK
      *
     C                   MOVEL     SCPGM1        ##OBJ
     C                   CALL      'SRCHKOBJ'    OBJPL
      *
     C     ##ERR         IFEQ      *ON
     C                   SETON                                        82  90
     C                   ENDIF                                                  ##ERR=*ON
      *
     C                   ENDIF                                                  IN90, SCPGM1 <> ' '
      *
     ** Program 2, if entered, must be valid
     C     *IN90         IFEQ      *OFF
     C     SCPGM2        ANDNE     *BLANK
      *
     C                   MOVEL     SCPGM2        ##OBJ
     C                   CALL      'SRCHKOBJ'    OBJPL
      *
     C     ##ERR         IFEQ      *ON
     C                   SETON                                        83  90
     C                   ENDIF                                                  ##ERR=*ON
      *
     C                   ENDIF                                                  IN90, SCPGM2 <> ' '
     C***  Begin Del  ***  04/04/08  **************************************
      ******BEG*ADD ******* 12/06/02 **********************************
      **********
     ***Program 3 must be blank if AUTO attach
     C********** *IN90     IFEQ *OFF
     C********** SCMNAU    ANDEQ'A'
     C********** SCPGM3    ANDNE*BLANK
     C**********           SETON                     33  90
     C**********           ENDIF
      ******END*ADD ******* 12/06/02 **********************************
     C***  End   Del  ***  04/04/08  **************************************
      *
     ** Program 3, if entered, must be valid
     C     *IN90         IFEQ      *OFF
     C     SCPGM3        ANDNE     *BLANK
      *
     C                   MOVEL     SCPGM3        ##OBJ
     C                   CALL      'SRCHKOBJ'    OBJPL
      *
     C     ##ERR         IFEQ      *ON
     C                   SETON                                        84  90
     C                   ENDIF                                                  ##ERR=*ON
      *
     C                   ENDIF                                                  IN90, SCPGM3 <> ' '
     C***  Begin Del  ***  04/04/08  **************************************
      **********
     C**********           ENDIF                           MNAU='A' OR PRNT='Y'
     C***  End   Del  ***  04/04/08  **************************************
      *
     ** State, if entered, must be valid
     C  N90SCST          IFNE      *BLANKS
     C                   MOVEL(P)  SCST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C   60              SETON                                        8590
     C                   ENDIF                                                  SCDESC
      *
     ** Single/Multiple form generation flag must be 'S' or 'M'
     C  N90SCSNGL        IFNE      'S'
     C     SCSNGL        ANDNE     'M'
     C**** BEGIN CHG **** 11/10/00 ************************************
     C******************** SETON                     79  90
     C                   SETON                                        87  90
     C**** END   CHG **** 11/10/00 ************************************
     C                   ENDIF                                                  SCSNGL <> N
      *
     ** Check to be sure screen has not been updated since entry
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
      *
     C  N90ENDT          IFGT      SAVEDT
     C     ENDT          OREQ      SAVEDT
     C     ENTIME        ANDGT     SAVETM
     C                   SETON                                        7190
     C                   ENDIF
      *
     ** We have passed all edits, goto the next screen
     C                   ENDSR
     *===============================================================*
     * ERRCK2 - Error checking for screen 2 (SCR02)
     *===============================================================*
     C     ERRCK2        BEGSR
      *
     ** Replaced by form must be on file
     C     *IN90         IFEQ      *OFF
     C     SCRPFM        ANDNE     *BLANK
     C                   MOVEL     SCRPFM        ##FORM
     C     ENKY          SETLL     PEN01                                  99
     C  N99              SETON                                        87  90
     C                   ENDIF                                                  IN90
      *
     ** Replacement date must be valid if entered
     C     *IN90         IFEQ      *OFF
     C     SCRPDT        ANDNE     *ZERO
      *
     C                   Z-ADD     SCRPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C     SRERR         IFEQ      *ON
     C                   SETON                                        88  90
     C                   ENDIF                                                  SRERR=*ON
      *
     C                   ENDIF                                                  IN90
      *
     ** Replacement form & date are mutually inclusive
     C     *IN90         IFEQ      *OFF
      *
     C     SCRPFM        IFNE      *BLANK
     C     SCRPDT        ANDEQ     *ZERO
     C     SCRPFM        OREQ      *BLANK
     C     SCRPDT        ANDNE     *ZERO
      ***** BEG DEL ******* 09/22/08 **********************************
     C*********            SETON                     89  90
      ***** END DEL ******* 09/22/08 **********************************
     C                   ENDIF                                                  SCRPFM <> ' '
      *
     C                   ENDIF                                                  IN90
     ** Check to be sure screen has not been updated since entry
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
      *
     C  N90ENDT          IFGT      SAVEDT
     C     ENDT          OREQ      SAVEDT
     C     ENTIME        ANDGT     SAVETM
     C                   SETON                                        7190
     C                   ENDIF
      *
     C                   ENDSR
     *===============================================================*
     * ERRCK3 - Error checking for screen 3 (SCR03)
     *===============================================================*
     C     ERRCK3        BEGSR
     C                   ENDSR
      *
      ***** BEG ADD ******* 12/06/02 **********************************
      *
      *
      *===============================================================
      * KJDO - SEARCH
      *===============================================================
      *
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
      *  State
      *
     C     HFLD          WHENEQ    'SCST'
     C                   CALL      'SY601R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *OFF          ##KC              1
     C                   PARM      *OFF          ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
      *
     C     ##RRN         IFEQ      *ZEROS
     C                   SETON                                        30
      *
     C                   ELSE
      *
     C     ##RRN         CHAIN     PST02                              60
     C                   MOVEL(P)  STKEY         SCST
     C                   SETON                                        31
     C                   ENDIF
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  KJDO
      *
      ***** END ADD ******* 12/06/02 **********************************
      *
      *
     *===============================================================*
     * MVDBSC - Move data base fields to screen fields
     *===============================================================*
     C     MVDBSC        BEGSR
      *
     C     *IN40         IFEQ      *OFF
      *
     C     *IN41         IFEQ      *OFF
     C     FNKY          CHAIN     PFN01                              99
     C  N99              MOVEL     FNNAME        SCFNNM
     C  N99              Z-ADD     FNFND         SCFND
      *
     C                   ELSE
      *
     C                   MOVE      *ZERO         SCFND
     C                   MOVE      *BLANKS       SCFNNM
     C                   ENDIF
      *
     C                   MOVEL     ENFORM        SCFORM
      *** beg chg ***** 11/10/16 ******************************
     C***********        MOVEL     ENDESC        SCDESC
     C                   MOVEL     ENDSC2        SCDESC
      *** end chg ***** 11/10/16 ******************************
     C                   MOVEL     ENST          SCST
      *
     C                   Z-ADD     ENEDDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEDDT
     C*** Begin Add *** 01/09/18 **************************************
     C                   Z-ADD     ENADTE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCADTE
     C*** End   Add *** 01/09/18 **************************************
     C*** begin add ***  05/16/17  ******************************************
     C                   Eval      scPathC = ENPathC
     C*** end add *****  05/16/17  ******************************************
      *
     C                   MOVEL     ENMNAU        SCMNAU
     C                   MOVEL     ENPRNT        SCPRNT
     C                   MOVEL     ENRPFM        SCRPFM
      *
     C                   Z-ADD     ENRPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRPDT
     C*** Begin Add *** 01/31/18 **************************************
     C                   MoveL(p)  ENformO       scFormO
     C                   MoveL(p)  ENnatFm       scNatFm
     C                   Movel(p)  ENexcWc       scExcWc
     C*** End   Add *** 01/31/18 **************************************
      *
     C                   MOVEL     ENFLDR        SCFLDR
     C                   MOVEL     ENPGM1        SCPGM1
     C                   MOVEL     ENPGM2        SCPGM2
     C                   MOVEL     ENPGM3        SCPGM3
     C                   MOVE      ENSTAT        SCSTAT
     C                   MOVEL     ENSNGL        SCSNGL
     C*** begin add ***  10/06/17  *******************************
     C                   MOVE      ENPEOF        SCPEOF
     C*** end add *****  10/06/17  *******************************
     C*** beg add *****  11/03/17  *******************************
     C                   MOVE      ENEXFL        SCEXFL
     C*** end add *****  11/03/17  *******************************
     C*** begin add ***  11/08/17  *******************************
     C                   MOVE      ENGPDF        SCGPDF
     C*** end add *****  11/08/17  *******************************
      *** Begin Add *** 02/28/17 ********************************
      /Free
        scEndPos = EndPos;
        //*** Beg Add ***** 08/20/18 ***************************
        scEnType = EnType;
        //*** End Add ***** 08/20/18 ***************************
      /End-Free
      *** End   Add *** 02/28/17 ********************************
      *
     C                   ELSE
      *
     C                   MOVEL     *BLANK        SCFORM
     C                   MOVEL     *BLANK        SCDESC
     C                   MOVEL     *BLANK        SCST
     C                   MOVEL     *ZERO         SCEDDT
     C*** begin add ***  05/16/17  ******************************************
     C                   Eval      scPathC = ' '
     C*** end add *****  05/16/17  ******************************************
     C                   MOVEL     *BLANK        SCMNAU
     C                   MOVEL     *BLANK        SCPRNT
     C                   MOVEL     *BLANK        SCRPFM
     C                   MOVEL     *ZERO         SCRPDT
     C*** Begin Add *** 01/31/18 **************************************
     C                   MoveL(p)  ' '           scFormO
     C                   MoveL(p)  ' '           scNatFm
     C                   Movel(p)  ' '           scExcWc
     C*** End   Add *** 01/31/18 **************************************
     C                   MOVEL     *BLANK        SCFLDR
     C                   MOVEL     *BLANK        SCPGM1
     C                   MOVEL     *BLANK        SCPGM2
     C                   MOVEL     *BLANK        SCPGM3
     C                   MOVEL     'A'           SCSTAT
     C                   MOVEL     *BLANK        SCSNGL
     C*** begin add ***  10/06/17  *******************************
     C                   MOVE      *blank        SCPEOF
     C*** end add *****  10/06/17  *******************************
     C*** begin add ***  11/08/17  *******************************
     C                   MOVE      *blank        SCGPDF
     C*** end add *****  11/08/17  *******************************
      *** Begin Add *** 02/28/17 ********************************
      /Free
        scEndPos = *blanks;
        //*** Beg Add ***** 08/20/18 ***************************
        scEnType = *blanks;
        //*** End Add ***** 08/20/18 ***************************
      /End-Free
      *** End   Add *** 02/28/17 ********************************
      *
     C                   ENDIF
      *
     C                   ENDSR
     *===============================================================*
     * MVSCDB - Move screen fields to data base fields
     *===============================================================*
     C     MVSCDB        BEGSR
      *
     C                   Z-ADD     ##CO#         ENCO#
     C                   Z-ADD     SCFND         ENFND
     C                   MOVE      SCFORM        ENFORM
      *** beg chg ***** 11/10/16 ***********************
     C***********        MOVEL     SCDESC        ENDESC
      /Free
       //*** Populate ENdesc for backwards compatibility. W1054R still
       //***  uses MUDESC to sort with (WMMUL3).

       //*** Begin Delete *** 05/23/17 ********************************
       //*** If ENdesc = *blanks;
       //***   ENdesc = scDesc;
       //*** Endif;
       //*** End Delete *** 05/23/17 ********************************

       //*** Begin Add *** 05/23/17 ********************************
       ENdesc = scDesc;
       //*** End    Add *** 05/23/17 ********************************

       ENdsc2 = scDesc;
      /End-Free
      *** end chg ***** 11/10/16 ***********************
     C                   MOVEL     SCST          ENST
      *
     C                   Z-ADD     SCEDDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENEDDT
     C*** Begin Add *** 01/09/18 **************************************
     C                   Z-ADD     SCADTE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENADTE
     C*** End   Add *** 01/09/18 **************************************
     C*** begin add ***  05/16/17  ******************************************
     C                   Eval      ENPathC = SCPathC
     C*** begin del ***  09/14/17  ******************************************
     C***                If        %subst(ENForm:1:4) <> 'WC99'
     C***                Clear                   ENPathC
     C***                Endif
     C*** end del *****  09/14/17  ******************************************
     C*** end add *****  05/16/17  ******************************************
      *
     C                   MOVEL     SCMNAU        ENMNAU
     C                   MOVEL     SCPRNT        ENPRNT
     C                   MOVEL     SCRPFM        ENRPFM
      *
     C                   Z-ADD     SCRPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENRPDT
      *
     C*** Begin Add *** 01/31/18 **************************************
     C                   MoveL(p)  scFormO       ENformO
     C                   MoveL(p)  scNatFm       ENnatFm
     C                   Movel(p)  scExcWc       ENexcWc
     C*** End   Add *** 01/31/18 **************************************
     C                   MOVEL     SCFLDR        ENFLDR
     C                   MOVEL     SCPGM1        ENPGM1
     C                   MOVEL     SCPGM2        ENPGM2
     C                   MOVEL     SCPGM3        ENPGM3
     C                   MOVEL     SCSTAT        ENSTAT
     C                   MOVEL     SCSNGL        ENSNGL
     C*** begin add ***  10/06/17  *******************************
     C                   MOVE      SCPEOF        ENPEOF
     C*** end add *****  10/06/17  *******************************
     C*** beg add *****  11/03/17  *******************************
     C                   MOVE      SCEXFL        ENEXFL
     C*** end add *****  11/03/17  *******************************
     C*** begin add ***  11/08/17  *******************************
     C                   MOVE      SCGPDF        ENGPDF
     C*** end add *****  11/08/17  *******************************
      *
      *** Begin Add *** 02/28/17 ********************************
      /Free
        EndPos = scEndPos;
        //*** Beg Add ***** 08/20/18 ***************************
        EnType = scEnType;
        //*** End Add ***** 08/20/18 ***************************
      /End-Free
      *** End   Add *** 02/28/17 ********************************
     C                   ENDSR
     *===============================================================*
     * OTHERR - Processing for other modes
     *===============================================================*
     C     OTHERR        BEGSR
     C                   ENDSR
     *===============================================================*
     * OTHFKS - Processing for other functions keys
     *===============================================================*
     C     OTHFKS        BEGSR
      *
      ***** BEG ADD ******* 12/06/02 **********************************
     C  N09*INKJ         CASEQ     *ON           KJDO                     90     F10=Search
     C                   ENDCS
      ***** END ADD ******* 12/06/02 **********************************
      *
     C                   ENDSR
     *===============================================================*
     * PGMEND - End of program processing
     *===============================================================*
     C     PGMEND        BEGSR
      ***** BEG ADD ******* 12/01/06 **********************************
     C                   SETON                                        LR
      ***** END ADD ******* 12/01/06 **********************************
     C                   ENDSR
     *===============================================================*
     * PGMSTR - Start of program processing
     *===============================================================*
     C     PGMSTR        BEGSR
      *
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C     @@MODE        COMP      'P'                                    41    COPY
      *
     C                   MOVEL(P)  OPTNM         ##DATA
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
      *
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
      *
     C                   MOVE      @@FND         ##FND
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     FNNAME        SCFNNM
     C                   Z-ADD     ##FND         SCFND
     C                   ENDIF
     C*** Begin Add *** 03/01/17 *********************************************
      /Free

       // Determine if this module get spool file and/or XML output
       procType = 'Policy Page';
       isUseSpoolOut = UseSpoolOut(procType);
       isUseXML = UseScriptura(procType) Or
                  Use3rdXml(procType);
       if isUseXML;
         isScriptUsed = 'Y';
         isUseScrip = *On;
       else;
         isScriptUsed = 'N';
         isUseScrip = *Off;
       Endif;

       If        isScriptUsed='Y' And isUseSpoolOut = *Off;
          skipDocEdit = *On;
       EndIf;

       //*** begin add ***  10/06/17  *******************************
       dspPEO = *off;
       selPEOF = ' ';
       choiPEOF = ' ';

       If FNPEOF <> ' ';
          dspPEO = *on;
          selPEOF = ' ,Master PEO,MCP PEO, Master or MCP PEO,' +
           'MCP Client,MCP PEO or MCP Client';
          choiPEOF = ' ,1,2,3,4,5';

       Endif;
       //*** end add *****  10/06/17  *******************************

       //*** beg add ***** 08/20/18  *******************************
       scEnTypSel = 'GTCO# = ' + %char(##CO#) +
        ' AND GTTBID = ''ENTP'' AND GTSTAT = ''A''';
       //*** end add ***** 08/20/18  *******************************

      /End-free
     C*** End   Add *** 03/01/17 ******************************************************
      *
     C                   ENDSR
     *===============================================================*
     * PREOTH - Pre-processing for other modes
     *===============================================================*
     C     PREOTH        BEGSR
      *
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
      *
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
      *
     C                   ENDSR
     *===============================================================*
     * UPDATR - Update data base record and after processing
     *===============================================================*
     C     UPDATR        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENDT
      *
     C                   MOVEL     DSTIME        ENTIME
      *
     C                   UPDATE    DBRCDF
     C                   ENDIF
      *
     C                   ENDSR
      *
     C***  Begin Add  ***  04/04/08  **************************************
     *===============================================================*
     * VALDOC - Validate document
     *===============================================================*
     C     VALDOC        BEGSR
     C*
     C                   EXSR      VALFLR
     C*
     C     *IN90         IFEQ      *OFF
      *
     C                   MOVEL(P)  SCFLDR        #2FOLD
      *
     C                   MOVEL(P)  SCFORM        #2TMP1            8
     C                   MOVE(P)   SCFORM        #2TMP2            7
     C                   MOVEL(P)  #2TMP1        #2DCID
      *
     C     #2TMP2        IFNE      *BLANK
     C                   CAT(P)    '.':0         #2DCID
     C                   CAT(P)    #2TMP2:0      #2DCID
     C                   ENDIF
      *
     C                   CALL      'SVALDOC'
     C                   PARM                    #2DCID           12
     C                   PARM                    #2FOLD           80
     C                   PARM      *BLANK        #2RTCD            1
      *
     C     #2RTCD        IFEQ      '1'
      *** Beg Add *** 06/16/14 *********************************************
     C     #2DCID        ANDNE     'MID-YEAR'
      *** End Add *** 06/16/14 *********************************************
     C                   SELECT
     ** If ADD mode, Document must exist in specified folder
     C     *IN40         WHENEQ    *ON                                          ADD MODE
     C     *IN41         OREQ      *ON                                          COPY MODE
     C                   SETON                                        88  90
     C                   OTHER
     C                   SETON                                        32  90
     C                   ENDSL
     C                   ENDIF                                                  QQRTCD IFEQ '1'
     C*
     C                   ENDIF
     C*
     C                   ENDSR
      *
     *===============================================================*
     * VALFLR - Validate folder
     *===============================================================*
     C     VALFLR        BEGSR
      *
     C                   MOVEL(P)  SCFLDR        #1FOLD
      *
     C                   CALL      'SVALFLR'
     C                   PARM                    #1FOLD           80
     C                   PARM      *BLANK        #1RTCD            1
      *
      *  Folder must exist and user must have authority to folder
      *
     C     #1RTCD        IFEQ      '1'
     C     #1RTCD        OREQ      '2'
     C                   SETON                                        78  90
     C                   ENDIF                                                  #1RTCD
      *
     C                   ENDSR
     C***  End   Add  ***  04/04/08  **************************************
      *
     *===============================================================*
     * WRITER - Write data base record and after processing
     *===============================================================*
     C     WRITER        BEGSR
      *
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENDT
      *
     C                   MOVEL     DSTIME        ENTIME
      *
     C                   WRITE     DBRCDF
     C                   FEOD      WTENP
      *
     C                   ENDSR
