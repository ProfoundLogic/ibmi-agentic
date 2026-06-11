     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      ************************************************************************/
      ************************************************************************/
      * SCI1S2R - SIS CLAIM  KEY, IMPORT PROCESSING PROGRAM                  */
      *                                                                      */
      * Description: CALLED FROM SCI105R - PROCESSES FILE IN IFS             */
      *                                                                      */
      * Created:     04/06/09                                                */
      * Programmer:  C:062                                                   */
      * Project:     115IAK                                                  */
      **************************************************************   *******/
      * Revisions:                                                           */
     *                                                                   *
     * 11/02/10  C:183 Project: SY195                                    *
     *                 Remove hard coding for QDLS. Call RTVDOCROOT to   *
     *                 get the root folder name.                         *
     *                                                                   *
      *              04/29/09                                                */
      * Programmer:  C:002                                                   */
      * Project:     115IAK                                                  */
      *
      *                                                                      */
      * -------------------------------------------------------------------- */
      *  Note: Please add comments to the top of revisions.                  */
      * -------------------------------------------------------------------- */
      ***************************************************************  *******/
     FSXFLP     IF   E           K DISK
     FSMI1P     IF   E             DISK
     FQSYSPRT1  O    F  198        PRINTER OFLIND(*INOA)
     F                                     USROPN

     D*
     D FND             S              3  0 DIM(132)                             FOUND ARRAY
     D R2              S              2    DIM(20)                              RTN CODES (2)ERR
     D R3              S              1    DIM(20)                              RTN CODES (3)SUCC
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D OBJIMP         UDS
     D  X#WSID                 1      2
     D*
     D @VR             C                   CONST('Error validating key-
     D                                      extracted from the -
     D                                     file name.          ')
     D*
     D*
     D @ER             C                   CONST('Error copying file t-
     D                                     o the Error director-
     D                                     y.                  ')
     D*
     D*
     D @DL             C                   CONST('Error deleting file -
     D                                     from the import dire-
     D                                     ctory.              ')
     D*
     D*
     D @DI             C                   CONST('Error Retrieving DI -
     D                                     and/or folder balanc-
     D                                     e info.  File not pr-
     D                                     ocessed.            ')
     D*
     D*
     D @QD             C                   CONST('Error copying file t-
     D                                     o QLDS file system. -
     D                                     File not processed. -
     D                                                         ')
     D*
     D*
     D @FB             C                   CONST('Error updating the f-
     D                                     ile and folder balan-
     D                                     ce info. File not pr-
     D                                     ocessed.            ')
     D*
     D*
     D @AR             C                   CONST('Error archiving the -
     D                                     file.               -
     D                                                         -
     D                                                         ')
     D*
     D                 DS
     D  XXHKEY                 1     40
     D  @@CLM#                 1     10
     D  @@BL                  11     40
      *** Beg add ***** 11/02/10 ***********************************************
     D RPGM            C                   CONST('RTVDOCROOT')
      *** End add ***** 11/02/10 ***********************************************

     I*

     C     FLKY          KLIST
     C                   KFLD                    @@TYPE           12
     C*
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*MDY'        SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     *DTAARA       DEFINE                  OBJIMP
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0
     C                   PARM                    RTNCD             2
     C*
     C                   MOVEL     '*STMF   '    @@TYPE           12
     C                   MOVEL     'OI'          @@WSID            2            DFLT WORK STN ID
     C*
     C*** RETRIEVE THE DATAAREA THAT HOLDS THE WSID USED FOR OBJIMPT
     C*
     C                   IN        OBJIMP                               12
     C*
     C*** IF THE VALUE EXISTS IN THE DATAAREA, USE IT, ELSE LEAVE THE DFT VALUE
     C*
     C     X#WSID        IFNE      *BLANKS
     C     *IN12         ANDEQ     *OFF
     C                   MOVEL     X#WSID        @@WSID
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
      *** Beg add ***** 11/02/10 ***********************************************
     C                   CALL      RPGM
     C                   PARM                    ROOT             10
      *** End add ***** 11/02/10 ***********************************************
     C*
     C     @@RRN         CHAIN     PI101                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '01'          RTNCD
     C                   GOTO      ENDIT
     C                   ENDIF
     C*
     C     FLKY          CHAIN     PFL01                              61
     C     *IN61         IFEQ      *ON
     C                   MOVE      '02'          RTNCD
     C                   GOTO      ENDIT
     C                   ENDIF
     C*
     C     *IN61         DOWEQ     *OFF
     C*
     C***   OPEN PRINT FILES
     C*
     C     XXPRT1        IFNE      '1'
     C                   OPEN      QSYSPRT1
     C                   MOVE      '1'           XXPRT1            1
     C                   EXCEPT    HEADE
     C                   ENDIF
     C*
     C*** CLEAR WORK FIELDS
     C*
     C                   MOVEA     *BLANKS       R3
     C                   MOVEA     *BLANKS       R2
     C                   MOVEA     *ZEROS        FND
     C                   MOVEL     *BLANKS       @@FULL
     C                   MOVEL     *BLANKS       @@EXTN
     C                   MOVEL     *BLANKS       @@DICD
     C                   MOVEL     *BLANKS       @@ERCD
     C                   MOVEL     *BLANKS       @@DLCD
     C                   MOVEL     *BLANKS       @@ARCD
     C                   MOVEL     *BLANKS       @@QDCD
     C                   MOVEL     *BLANKS       @@FBCD
     C                   MOVEL     *BLANKS       @@VRCD
     C                   MOVE      *BLANKS       @@TDIR
     C                   MOVE      *BLANKS       XXTDIR
     C                   MOVE      *ZEROS        E1
     C                   MOVE      *ZEROS        E2
     C                   MOVE      *ZEROS        E3
     C                   MOVE      *ZEROS        E4
     C                   MOVE      *ZEROS        S2
     C                   MOVE      *ZEROS        S3
     C                   MOVE      *ZEROS        S4
     C                   MOVE      *ZEROS        L2
     C                   MOVE      *ZEROS        L3
     C                   MOVE      *ZEROS        L4
     C                   MOVE      *ZEROS        XT
     C                   MOVEA     '00000000'    *IN(90)
     C                   MOVEA     '00'          *IN(98)
     C                   MOVEL     *BLANKS       @@BL
     C                   MOVEL     *BLANKS       XXHKEY
     C                   MOVEL     *BLANKS       XXHDT
     C                   MOVEL     *BLANKS       XXHTM
     C                   MOVEL     *BLANKS       XXHDSC
     C*
     C                   EXSR      PRCFIL
     C*
     C***   LOOP TO NEXT SXFL RECORD
     C*
     C     FLKY          READE     PFL01                                  61
     C*
     C                   ENDDO
     C*
     C*
     C* End the program
     C*
     C     ENDIT         TAG
      *
     C                   MOVE      'Y'           QQINLR
     C                   EXSR      WRTLOG
      *
     C                   MOVE      *ON           *INLR
      *****************************************************************
      * * *     SUBROUTINE - PRCFIL - PROCESS FILES FROM IFS      * * *
      *****************************************************************
     C     PRCFIL        BEGSR
     C*
     C***   FOR EACH RCD,
     C***     SUBROUTINE TO EXTRACT KEY VALUE FROM FILE NAME
     C***     EXTRACT KEY
     C*
     C                   EXSR      NAMFLD
     C*
     C*** NON-SIS REF IS ONE KEY FIELD OF 20A (DONT NEED TO BREAK OUT)
     C*
     C                   MOVE      *BLANKS       @@KEY
     C                   MOVEL     XXHKEY        @@KEY            40
     C*
     C*** CONVERT DATE/TIME TO USEFUL VALUES
     C*
     C* DATE TO MMDDCCYY
     C*
     C                   MOVE      *ZEROS        SRDTI
     C                   MOVE      XXHDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         @@CDAT            8            DT FOR API
     C*
     C* TIME AS IS 6A
     C*
     C                   MOVE      *ZEROS        @@CTIM
     C                   MOVEL     XXHTM         @@CTIM            6
     C*
     C* DESC AS IS 132 (AS COMMENT IN DARTS, OR MOREOBJECTS)
     C*
     C                   MOVE      *BLANKS       @@DESC
     C                   MOVEL     XXHDSC        @@DESC          132
     C*
     C***        VALIDATE KEY IN SAME SUBROUTINE
     C***  VALIDATE KEY
     C*
     C                   CALL      'ISIV62R'                                    COULD GET FRM FIL
     C                   PARM                    @@KEY            40
     C                   PARM                    @@RTNC            1
     C*
     C***        NOT VALID
     C*
     C     @@RTNC        IFNE      ' '
     C                   MOVEL     @@RTNC        @@VRCD            2
     C*
     C*** EXECUTE ERROR SUBROUTINE ***
     C*
     C                   EXSR      ERROR1
     C*
     C* @@ERCD-COPY TO ERROR DIRECTORY, @@DLCD-DELETE FROM ORIGINAL DIRECTRY
     C*
      *** FORMAT THE ERROR REPORT DETAIL LINE HERE ***
      *
     C                   ELSE
      *
      *  VALID -  GET NEW NAME FOR THE FILE (QDLS/FOLDER BALANCE)
      *
     C                   CALL      'SCI107R'
     C                   PARM                    @@WSID
     C                   PARM      '000000'      @@DSEQ            6
     C                   PARM      *BLANKS       @@DSFX            3
     C                   PARM      '0000'        @@BSEQ            4
     C                   PARM      *BLANKS       @@IDOC           12
     C                   PARM      *BLANKS       @@IBCH           12
     C                   PARM      *BLANKS       @@IFLR            8
     C                   PARM      *BLANKS       @@ISFL           12
     C                   PARM                    @@EXTN            4
     C                   PARM      ' '           @@RTN3            1
      *
      *** IF RTN = 1, DOC INDEX RECORD NO FOUND FOR THIS WSID (OI)
      *
     C     @@RTN3        IFNE      *BLANKS
     C                   MOVEL     @@RTN3        @@DICD            1
     C                   ENDIF
      *
      ***     ONLY PROCESS IF QDLS INFO IS RETRIEVED
     C     @@RTN3        IFEQ      *BLANK
      *
      ***             COPY OBJECT FROM THE IFS FOLDER TO THE NEW QDLS FOLDER
      *
      ***    BUILD TOFLR/OBJ INFO FOR CPYOBJ API CALL
      *
     C                   MOVE      *BLANKS       @@WK1
     C                   MOVE      *BLANKS       @@WK2
     C                   MOVE      *BLANKS       @@WK3
      *** Beg chg ***** 11/02/10 ***********************************************
     C*******    '\QDLS\'  CAT  @@IFLR:0  @@WK1  14
     C     '\'           CAT(P)    ROOT          RPATH            12
     C     RPATH         CAT(P)    '\':0         RPATH
     C     RPATH         CAT       @@IFLR:0      @@WK1            16
      *** End chg ***** 11/02/10 ***********************************************
     C     @@WK1         CAT       '\':0         @@WK2            17
     C     @@WK2         CAT       @@ISFL:0      @@WK3            29
     C*
     C                   CALL      'CPYOBJC'
     C                   PARM      I1IDIR        @@FDIR           80
     C                   PARM      FLOBJN        @@FOBJ          132
     C                   PARM      @@WK3         @@TDIR           80
     C                   PARM      @@IDOC        @@TOBJ          132
     C                   PARM      '  '          @@RTN2            2
     C*
     C     '\'           CAT(P)    @@TOBJ:0      WK133           133
     C     @@TDIR        CAT(P)    WK133:0       WK212           212
     C                   MOVEL     WK212         XXTDIR           60
      *
      *  ** IF RTN CODE  THEN PRINT ON REPORT, NO SUCCESSFUL COPY TO QDLS
      *
     C     @@RTN2        IFNE      *BLANKS
     C                   MOVEL     @@RTN2        @@QDCD            2
     C                   ELSE
     C                   MOVEL     *BLANKS       @@QDCD
     C                   ENDIF
      *
     C     @@QDCD        IFEQ      *BLANKS
      *
      ***             CALL PGM TO UPDATE THE DI AND FB RCDS.. NEXT AVAIL
      *
     C                   CALL      'SCI106R'
     C                   PARM                    @@WSID
     C                   PARM                    @@DSEQ            6
     C                   PARM                    @@BSEQ            4
     C                   PARM                    @@IFLR
     C                   PARM      ' '           @@RTN4            1
      *
     C     @@RTN4        IFNE      *BLANKS
     C                   MOVEL     @@RTN4        @@FBCD            2
     C                   ELSE
     C                   MOVEL     *BLANKS       @@FBCD
     C                   ENDIF
      *
     C     @@FBCD        IFEQ      *BLANKS
      *
      ***             WRITE A DART &/OR MORE OBJECT RECORD
     C*** DETERMINE WHICH TYPE OF RECORD WE ARE WRITING...
     C*
     C     I1IDXT        CASEQ     'D'           DRTRCD                         IF MAJOR DART
     C     I1IDXT        CASEQ     'M'           MORCD                          IF MORE OBJ RCD
     C                   CAS                     MORCD                          ANY OTHER
     C                   ENDCS
      *
      ***             MOVE OBJECT TO THE ARCHIVE FOLDER  (IFS)
      *
     C     I1ADIR        IFNE      *BLANKS
      *
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        $$DAT             6
     C                   MOVE      DSTIME        $$TIM             6
     C                   MOVEL     $$DAT         $$DTTM           12
     C                   MOVE      $$TIM         $$DTTM
     C*
     C     $$DTTM        CAT(P)    FLOBJN:0      $$NOBJ          132
     C*
     C                   CALL      'CPYOBJC'
     C                   PARM      I1IDIR        @@FDIR           80
     C                   PARM      FLOBJN        @@FOBJ          132
     C                   PARM      I1ADIR        @@TDIR           80
     C                   PARM      $$NOBJ        @@TOBJ          132
     C                   PARM      '  '          @@RTN2            2
      *
      *  ** IF RTN CODE  THEN PRINT ON REPORT, NO ARCHIVE!!!
      *
     C     @@RTN2        IFNE      *BLANKS
     C                   MOVEL     @@RTN2        @@ARCD            2
     C                   ELSE
     C                   MOVEL     *BLANKS       @@ARCD
     C                   ENDIF
      *
     C                   ENDIF                                                   IF I1ADIR
      *
     C     @@ARCD        IFEQ      *BLANKS
      *
      *** ONLY DELETE IF THE COPY ABOVE WAS SUCCESSFUL (TO ERROR DIRECTORY)
      *
     C                   CALL      'DELOBJC'
     C                   PARM      I1IDIR        @@FDIR           80
     C                   PARM      FLOBJN        @@FOBJ          132
     C                   PARM      '  '          @@RTN2            2
      *
     C     @@RTN2        IFNE      *BLANKS
     C                   MOVEL     @@RTN2        @@DLCD            2
     C                   ELSE
     C                   MOVEL     *BLANKS       @@DLCD
     C                   ENDIF
      *
     C                   ENDIF                                                  @@ARCD
      *
     C                   ENDIF                                                  IF FB OK  (FBCD)
     C                   ENDIF                                                  IF QDLS OK (QDCD)
     C*
     C                   ELSE
     C*** ERROR ON DI RECORD... COULDN'T CONTINUE... ERROR RPT???
      *
     C                   ENDIF
     C                   ENDIF
     C*
     C*** Error or Success Report here
     C*
     C     @@DICD        IFNE      *BLANK
     C     @@ERCD        ORNE      *BLANK
     C     @@QDCD        ORNE      *BLANK
     C     @@FBCD        ORNE      *BLANK
     C     @@ARCD        ORNE      *BLANK
     C     @@DLCD        ORNE      *BLANK
     C     @@VRCD        ORNE      *BLANK
     C*
     C***   ERROR REPORT
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEADE
     C                   ENDIF
     C*
     C                   EXCEPT    ERR1
     C*
     C*** LOOP FOR THE ERRORS
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEADE
     C                   EXCEPT    ERR1
     C                   ENDIF
      *
     C     @@DICD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@DICD        $$ERCD            2
     C                   MOVEL     @DI           $$ERDS           80
     C                   MOVE      'U'           QQSTS
     C                   EXSR      WRTLOG
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C*
     C     @@ERCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@ERCD        $$ERCD            2
     C                   MOVEL     @ER           $$ERDS           80
     C                   MOVE      'U'           QQSTS
     C                   EXSR      WRTLOG
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C     @@QDCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@QDCD        $$ERCD            2
     C                   MOVEL     @QD           $$ERDS           80
     C                   MOVE      'U'           QQSTS
     C                   EXSR      WRTLOG
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C     @@FBCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@FBCD        $$ERCD            2
     C                   MOVEL     @FB           $$ERDS           80
     C                   MOVE      'U'           QQSTS
     C                   EXSR      WRTLOG
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C     @@ARCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@ARCD        $$ERCD            2
     C                   MOVEL     @AR           $$ERDS           80
     C                   MOVE      'U'           QQSTS
     C                   EXSR      WRTLOG
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C     @@DLCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@DLCD        $$ERCD            2
     C                   MOVEL     @DL           $$ERDS           80
     C                   MOVE      'U'           QQSTS
     C                   EXSR      WRTLOG
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C     @@VRCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@VRCD        $$ERCD            2
     C                   MOVEL     @VR           $$ERDS           80
     C                   MOVE      'U'           QQSTS
     C                   EXSR      WRTLOG
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C                   EXCEPT    ERR3
     C*
     C                   ELSE
     C*
     C***   WRITE SUCCESS REPORT
     C*
     C                   MOVE      'S'           QQSTS
     C                   EXSR      WRTLOG
     C                   ENDIF
     C*
     C                   ENDSR
      *****************************************************************
      * * *     SUBROUTINE - NAMFLD - BREAK FILE NAME TO FIELDS   * * *
      *****************************************************************
     C     NAMFLD        BEGSR
      *
      *** CLEAR WORK FIELDS
      *
     C                   MOVEL     FLOBJN        @@FULL          132
      *
      *** EXTRACT THE KEY, DATE, TIME, DESCRIPTION FROM THE FULL OBJ NAME
      *                    FND IS AN ARRAY AND CONTAINS ALL OCCURANCES OF
      *                    " "
     ** FOR SIS CLAIMS ON THESE FILE NAMES, FAC HAS THE KEY STRUCTURE AS
      * FOLLOWS:
      *         USELES             CHAR 17
      *         CLAIM#             CHAR 10
      *         SPACE              CHAR 1
      *         DATE               CHAR 6    MMDDYY
      *         SPACE              CHAR 1
      *         TIME               CHAR 6    HHMMSS
      *         SPACE              CHAR 1
      *         DOCUMENT NAME      VARIABLE BY DOCUMENT NAME
      *         SUFFIX                 .PDF
     C*
     C* *** CLAIM#
     C*
     C***** DEL 4/28/09 ** MOVEL@@FULL    @@CLM# 10 P
     ***** ADD 4/28/09 **********************************
     C* *** CLAIM#
     C     10            SUBST(P)  @@FULL:18     @@CLM#           10
     C*
     ***** END ADD *** 4/29/09 *********************
     C*
     C*** FIND FIRST BLANK AFTER THE KEY (POS 28+)
     C*
     C     ' '           SCAN      @@FULL:28     FND                      90
     C*
     C*** FIND SUFFIX
     C*
     C     '.'           SCAN      @@FULL:1      XT                3 0    99      FILE EXTN STRT
     C*
     C*** GET EXTENTION TO FILE
     C*
     C     4             SUBST     @@FULL:XT     @@EXTN            4              FILE EXTNTN
     C                   MOVEL     @@EXTN        WK1               1
     C     WK1           IFNE      '.'
     C                   MOVEL     '.'           @@EXTN
     C                   MOVE      'OBJ'         @@EXTN
     C                   ENDIF
     C*
     C     FND(1)        SUB       1             E1                3 0
     C     E1            IFGT      0
     C*
     C*  EXTRACT THE DATE
     C*
     C     FND(1)        ADD       1             S2                3 0          START OF 2ND PART
     C     FND(2)        SUB       S2            L2                3 0          LENGTH OF 2ND PRT
     C     FND(2)        SUB       1             E2                3 0          END OF 2ND PRT
     C*
     C     L2            SUBST     @@FULL:S2     XXHDT             6    92      HOLD DATE
     C*
     C*  EXTRACT THE TIME
     C*
     C     FND(2)        ADD       1             S3                3 0          START OF 3RD PART
     C     FND(3)        SUB       S3            L3                3 0          LENGTH OF 3RD PRT
     C     FND(3)        SUB       1             E3                3 0          END OF 3RD PRT
     C*
     C     L3            SUBST     @@FULL:S3     XXHTM             6    93      HOLD TIME
     C*
     C*  EXTRACT THE DESCRIPTION
     C*
     C     *IN99         IFEQ      *OFF                                         OF @@FULL OR NO
     C                   Z-ADD     133           FND(4)                         EXTNTN FOUND..
     C                   ELSE
     C                   Z-ADD     XT            FND(4)                         ONLY ONE '.'
     C                   ENDIF
     C*
     C     FND(3)        ADD       1             S4                3 0          START OF 3RD PART
     C     FND(4)        SUB       S4            L4                3 0          LENGTH OF 3RD PRT
     C     FND(4)        SUB       1             E4                3 0          END OF 3RD PRT
     C*
     C     L4            SUBST     @@FULL:S4     XXHDSC          132    94      HOLD DESCRIPTION
      *
     C                   ENDIF
      *
     C                   ENDSR
     C*****************************************************************
     C* * *     SUBROUTINE - ERROR1 - ERROR SUBROUTINE 1          * * *
     C*****************************************************************
     C     ERROR1        BEGSR
      *
      ***           MOVE TO ERROR DIRECTORY
      *
     C     I1EDIR        IFNE      *BLANKS
      *
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        $$DAT             6
     C                   MOVE      DSTIME        $$TIM             6
     C                   MOVEL     $$DAT         $$DTTM           12
     C                   MOVE      $$TIM         $$DTTM
     C*
     C     $$DTTM        CAT(P)    FLOBJN:0      $$NOBJ          132
     C*
      *
     C                   CALL      'CPYOBJC'
     C                   PARM      I1IDIR        @@FDIR           80
     C                   PARM      FLOBJN        @@FOBJ          132
     C                   PARM      I1EDIR        @@TDIR           80
     C                   PARM      $$NOBJ        @@TOBJ          132
     C                   PARM      '  '          @@RTN2            2
      *
      *** IF RTNCD, PRINT ON ERROR REPORT ALSO
      *
     C     @@RTN2        IFNE      *BLANKS
     C                   MOVEL     @@RTN2        @@ERCD            2
     C                   ELSE
     C                   MOVEL     *BLANKS       @@ERCD
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     @@ERCD        IFEQ      *BLANKS
      *
      *** ONLY DELETE IF THE COPY ABOVE WAS SUCCESSFUL (TO ERROR DIRECTORY)
      *
     C                   CALL      'DELOBJC'
     C                   PARM      I1IDIR        @@FDIR           80
     C                   PARM      FLOBJN        @@FOBJ          132
     C                   PARM      '  '          @@RTN2            2
      *
     C     @@RTN2        IFNE      *BLANKS
     C                   MOVEL     @@RTN2        @@DLCD            2
     C                   ELSE
     C                   MOVEL     *BLANKS       @@DLCD
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDSR
      *****************************************************************
      * * *     SUBROUTINE - DRTRCD - CREATE DART RECORD FOR IMPT * * *
      *****************************************************************
     C     DRTRCD        BEGSR
      *
     C*** WRITE REMARK RECORD
     C*
     C*  *** PROGRAM NAME TO USE FOR PNAM - KEY STRUCTURE
     C                   MOVEL     'SISCLM  '    ##PNAM           10
     C*          ------------------------------------------------------
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  @@KEY         Q#KEY            40
     C*** DETERMINE THE COMMENT FIELD
     C                   MOVEL     I1DFTT        XXWK5             5
     C     XXWK5         IFEQ      '*FILE'
     C*** USE ONLY FILE NAME FOR REMARK TEXT
     C                   MOVEL(P)  FLOBJN        Q#COMM          256
     C                   ELSE
     C     I1DFTT        IFEQ      *BLANKS
      *** PUT IN RMCD TEXT
     C                   CALL      'DA200R'
     C                   PARM      I1RMCD        #@RMCD            5
     C                   PARM      *BLANKS       #@DSC            40
     C                   PARM      *BLANK        #@RTN             1
      *
     C     #@RTN         IFEQ      'Y'
     C                   MOVEL(P)  #@DSC         Q#COMM
     C                   ELSE
     C                   MOVEL(P)  FLOBJN        Q#COMM          256
     C                   ENDIF
     C                   ELSE
     C*** USE VALUE *CAT FILENAME
     C     I1DFTT        CAT(P)    FLOBJN:3      WK175           175
     C                   MOVEL(P)  WK175         Q#COMM
     C                   ENDIF                                                  IF = *BLANKS
     C                   ENDIF                                                  IF = *FILE
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   MOVEL(P)  @@CDAT        Q#CDAT            8
     C                   MOVEL(P)  ##PNAM        Q#PNAM           10
     C     I1IDXT        IFEQ      'M'
     C                   MOVEL     *BLANKS       Q#DCID
     C                   MOVEL     *BLANKS       Q#IMGF
     C                   ELSE
     C                   MOVEL(P)  I1IMGF        Q#IMGF            1
     C                   MOVEL(P)  @@IDOC        Q#DCID           12
     C                   ENDIF
     C*** COULD HAVE SECURITY VALUE FROM SMI1P?????
     C                   MOVE      *ZEROS        ##SECL            3
     C*** SECURE BY LEVEL = 'N' BECAUSE WE CANT SECURE WITHOUT SMI1P FILE
     C                   MOVEL(P)  'N'           Q#SBYL            1
      *
     C                   MOVE      '000'         Q#SECL            3
     C                   MOVE      *BLANKS       SCDUSR           10
     C*** COULD HAVE DIARY USER IN THE SMI1P FILE
     C     SCDUSR        IFNE      *BLANKS
     C                   MOVEL(P)  SCDUSR        Q#USER           10
     C                   ELSE                                                   DUSR = blanks
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   ENDIF                                                  DUSR = blanks
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C     @@IFLR        CAT       '/':0         WK9               9
     C     WK9           CAT       @@ISFL:0      WK21             21
     C                   MOVEL(P)  WK21          Q#FOLD           80
     C                   MOVE      *BLANKS       SCDITP            1
     C*** IF DIARY USER ENTERED IN THE SMI1P FILE, DIARY WOULD = 'Y'
     C     SCDITP        IFEQ      'Y'
     C     SCDITP        OREQ      'N'
     C                   MOVE      SCDITP        Q#DIFL            1
     C                   ELSE                                                   DITP = 'Y'
     C                   MOVE      *BLANK        Q#DIFL            1
     C                   ENDIF                                                  DITP = 'Y'
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  'N'           Q#FL02            1            PURGABLE
     C                   MOVEL(P)  I1RMCD        Q#RMCD            5
     C                   MOVE      '00000'       Q#SEQ#            5
     C*** APPLICATION ID?? WHAT???
     C                   MOVEL(P)  'WC'          Q#APID            2
      *
     C     I1IMGF        IFEQ      'I'
     C                   MOVEL(P)  @@IDOC        Q#IDOC           12
     C                   ENDIF
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  I1IMGF        Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *BLANKS       Q#PIDX            5
     C                   MOVEL(P)  *BLANKS       Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   MOVEL(P)  @@KEY         QQKEY            40
     C                   MOVE      Q#SEQ#        ##SEQ#            5 0
     C                   MOVEL     *BLANKS       QQRCOD            1
     C*
     C                   ENDSR
      *****************************************************************
      * * *     SUBROUTINE - MORCD - CREATE MORE OBJ RECORD       * * *
      *****************************************************************
     C     MORCD         BEGSR
      *
      *** 1ST CREATE THE MAJOR REMARK RECORD
      *
     C                   EXSR      DRTRCD
      *
     C     I1IMGF        IFEQ      'W'
     C     I1IMGF        OREQ      ' '
     C                   MOVEL     'D'           $$OTYP
     C                   ELSE
     C                   MOVEL     I1IMGF        $$OTYP
     C                   ENDIF
     C*
     C                   CALL      'ISEWMD'
     C                   PARM      @@KEY         $$KEY            40
     C                   PARM      Q#SEQ#        $$SEQ             5
     C*
     C                   PARM      @@IDOC        $$OID            40
     C                   PARM      Q#FOLD        $$OFLR           60
     C                   PARM                    $$OTYP            1
     C                   PARM      FLOBJN        $$ODSC           40
     C                   PARM      Q#RMCD        $$RMCD            5
     C                   PARM      'AS400   '    $$SYSN           20
     C                   PARM      ##PNAM        $$PNAM           10
     C                   PARM      Q1USER        $$USR            10
     C                   PARM      ' '           $$RTN             1
     C                   PARM      *BLANKS       $$ERTN           80
     C*
      *
     C                   ENDSR
      *****************************************************************
      * * *     SUBROUTINE - MORCD - CREATE MORE OBJ RECORD       * * *
      *****************************************************************
     C     WRTLOG        BEGSR
      *
     C                   MOVE      *BLANKS       QQADIR
     C                   MOVE      *BLANKS       QQEDIR
     C                   MOVE      *BLANKS       QQERCD
     C                   MOVE      *BLANKS       QQERDE
      *
     C                   MOVE      $$DAT         QQDATE
     C                   MOVE      $$TIM         QQTIME
      *
     C     QQSTS         IFEQ      'S'
     C                   MOVE      I1ADIR        QQADIR
     C                   MOVEL(P)  I1ADIR        QQTDIR
     C                   ENDIF
      *
     C     QQSTS         IFEQ      'U'
     C                   MOVE      I1EDIR        QQEDIR
     C                   MOVE(P)   I1EDIR        QQTDIR
     C                   MOVE      $$ERCD        QQERCD
     C                   MOVE      $$ERDS        QQERDE
     C                   ENDIF
      *
     C                   CALL      'DA002R'
     C                   PARM                    QQDATE            6 0
     C                   PARM                    QQTIME            6 0
     C                   PARM      I1IDIR        QQFDIR           80
     C                   PARM      FLOBJN        QQFFIL          132
     C                   PARM                    QQEDIR           80
     C                   PARM                    QQADIR           80
     C                   PARM                    QQTDIR           80
     C                   PARM      $$NOBJ        QQTFIL          132
     C                   PARM                    QQERCD            2
     C                   PARM                    QQERDE           80
     C                   PARM                    QQSTS             1
     C                   PARM      Q1PGM         QQPGM            10
     C                   PARM                    QQINLR            1
      *
     C                   ENDSR
     O*
     OQSYSPRT1  E            HEADE          1 06
     O                                          111 'OBJECT IMPORT ERROR LIST'
     O                       DSDATE        Y     12
     O                       DSTIME             196 '  :  :  '
     O          E            HEADE          3
     O                                           14 'FROM DIR:'
     O                       I1IDIR              95
     O*
     O                                          110 'ERROR DIR:'
     O                       I1EDIR             191
      *
     O          E            HEADE          1
     O                                           10 'FILE NAME'
     O                                          144 'TO FILE'
     O          E            ERR1           1
     O                       FLOBJN             133
     O                       XXTDIR             198
     O          E            ERR2           1
     O                                            8 '     ***'
     O                       $$ERCD              11
     O                                           12 '-'
     O                       $$ERDS              92
     O          E            ERR3           1
