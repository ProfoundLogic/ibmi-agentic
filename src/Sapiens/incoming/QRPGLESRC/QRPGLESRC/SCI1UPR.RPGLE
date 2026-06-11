     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      ************************************************************************/
     * StoneRiver is a trademark of StoneRiver, Inc.                 *
      ************************************************************************/
      * SCI1UPR - SUMMIT POL KEY-BOTH, IMPORT PROCESSING PROGRAM             */
      *                                                                      */
      * Description: CALLED FROM SCI105R - PROCESSES FILE IN IFS             */
      **** FILE NAME: INCOMING
      *       PRCS BOTH KEYS TO DARTS:
      *               FYR    3
      *               CARR   4
      *               CLNT   5
      *                        EXAMPLE: 103018700010 XXXXXXXXXXX.PDF
      *                         THIS WOULD CREATE 2 DART RCDS
      *                         1)  KEY = 00118700010103
      *                         2)  KEY = 00118700010
      *       PRCS ONLY ONE KEY TO DARTS:
      *               CARR   4
      *               CLNT   5
      *                        EXAMPLE: 018700010 XXXXXXXXXXX.PDF
      *                         THIS WOULD CREATE 1 DART RCDS
      *                         1)  KEY = 00118700010
      *                                                                      */
      * Created:     01/28/02                                                */
      * Programmer:  C:128                                                   */
      * Project:     10IBS                                                   */
      **************************************************************   *******/
      * Revisions:                                                           */
     *                                                                   *
     * 11/02/10  C:183 Project: SY195                                    *
     *                 Remove hard coding for QDLS. Call RTVDOCROOT to   *
     *                 get the root folder name.                         *
      *
      *    01/28/10 C:249 PROJECT: SY215
      *             C:189 CHANGE BRAND NAME.
      *
      *    05/30/08 C:62 PROJECT: DR257
      *                  ADD ROLLOVER PROCESSING ERROR MESSAGES.
      *
      *    53 - 06/19/06 - CALL API DA002R TO WRITE LOG RECORDS.  REMOVE
      *                    OUTPUT FOR SUCESSFULL IMPORTS.
      **
      *
      *    02 - 08/30/05 - CHANGE TO GET THE WSID USED BY THE PROCESS FROM A
      *                    DATAAREA (OI).  WILL EXTEND THE LIMIT ON # OF OBJ
      *                    IMPORTED.
      *
      **   02 - 01/05/04 - FIX ON WRITING OF BOTH KEYS....
      **   02 - 12/29/03 - WRITE BOTH KEYS IF YEAR PRESNET
      *    02 - 09/24/03 - CHANGE TO AVOID BOMB ON NO EXTENTION IN FILE
      **
      *    02 - 02/27/03 - CHANGE FILE COPY TO ARCHIVE AND ERROR TO INC.
      *                    DATE/TIME STAMP AS PART OF OBJECT NAME.
      *                                                                      */
      *                                                                      */
      * -------------------------------------------------------------------- */
      *  Note: Please add comments to the top of revisions.                  */
      * -------------------------------------------------------------------- */
      ***************************************************************  *******/
      *
     FSXFLP     IF   E           K DISK
     FSMI1P     IF   E             DISK
     FQSYSPRT1  O    F  198        PRINTER OFLIND(*INOA)
     F                                     USROPN
      *** BEG DEL *** 06/19/06 ****************************
     F*QSYSPRT2O***F     198     OB     PRINTER                        UC
      *** END DEL *** 06/19/06 ****************************
     F*  QSYSPRT1 IS THE REPORT FOR ERRORS, QSYSPRT2 IS THE REPORT FOR
     F*    SUCCESSFUL

     D FND             S              3  0 DIM(132)                             FOUND ARRAY
     D R2              S              2    DIM(20)                              RTN CODES (2)ERR
     D R3              S              1    DIM(20)                              RTN CODES (3)SUCC
      *
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D                 DS
     D  XXHKEY                 1     40
     D  #$FYR                  1      3
     D  #$CARR                 4      7
     D  #$CLNT                 8     12
     D  #$CLR                 13     40
     D*
      *
     D                 DS
     D  XXHKY1                 1     40
     D*** DEL 01/05/04 *********              1   4 #$CAR1
     D*** DEL 01/05/04 *********              5   9 #$CLN1
     D*** DEL 01/05/04 *********             10  40 #$CLR1
     D*** ADD 01/05/04 *************************************************
     D*************************************** 1   3 #$FYR1
     D  #$CAR1                 1      4
     D  #$CLN1                 5      9
     D  #$CLR1                10     40
     D*** END ADD 01/05/04 *********************************************
     D*
     D                 DS
     D  NWKEY                  1     40
     D  #@CO#                  1      3
     D  #@CARR                 4      6
     D  #@CLNT                 7     11
     D  #@FYR                 12     14
     D*
     D*
     D                 DS
     D  NWKY1                  1     40
     D  #@CO#1                 1      3
     D  #@CAR1                 4      6
     D  #@CLN1                 7     11
     D*
     D*
     D                 DS
     D  LLKEY                  1     40
     D  LLCO#                  1      3
     D  LLCARR                 4      6
     D  LLCLNT                 7     11
     D*
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D** ADD 08/30/05 *******************************************************
     D OBJIMP         UDS
     D  X#WSID                 1      2
     D** END ADD 08/30/05 *****************************************************
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
     D*** BEGIN ADD *** 05/30/08 **************************************
     D*
     D*** BEGIN DEL *** 01/28/10 **************************************
     D**            'Document indexing ha-C         @RO
     D**            's rolled.  Please co-
     D**            'ntact Fiserv support-
     D**            '.                   '
     D*
     D*** END   DEL *** 01/28/10 **************************************
     D @R2             C                   CONST(' support.')
     D COMAPI          C                   CONST('SA005R')
     D*
     D*** END   ADD *** 05/30/08 **************************************
      *** Beg add ***** 11/02/10 ***********************************************
     D RPGM            C                   CONST('RTVDOCROOT')
      *** End add ***** 11/02/10 ***********************************************
     i*
      *
      *

     C     FLKY          KLIST
     C                   KFLD                    @@TYPE           12
      *
      *
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*MDY'        SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*** ADD 08/30/05 *******************************************************
     C*
     C     *DTAARA       DEFINE                  OBJIMP
      *
     C*** END ADD 08/30/05 ****************************************************
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0
     C                   PARM                    RTNCD             2
     C*
     C                   MOVEL     '*STMF   '    @@TYPE           12
     C                   MOVEL     'OI'          @@WSID            2            DFLT WORK STN ID
     **** ADD 08/30/05 ***************************************************
      *
      *** RETRIEVE THE DATAAREA THAT HOLDS THE WSID USED FOR OBJIMPT
     C                   IN        OBJIMP                               12
      *** IF THE VALUE EXISTS IN THE DATAAREA, USE IT, ELSE LEAVE THE DFT VALUE
     C     X#WSID        IFNE      *BLANKS
     C     *IN12         ANDEQ     *OFF
     C                   MOVEL     X#WSID        @@WSID
     C                   ENDIF
      *
     **** END ADD 08/30/05 ***************************************************
     C*
     C                   TIME                    DSDTTM
      *** Beg add ***** 11/02/10 ***********************************************
     C                   CALL      RPGM
     C                   PARM                    ROOT             10
      *** End add ***** 11/02/10 ***********************************************
     C*
      *
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
      *
     C     *IN61         DOWEQ     *OFF
      *
      ***   OPEN PRINT FILES
      *
     C     XXPRT1        IFNE      '1'
     C                   OPEN      QSYSPRT1
     C                   MOVE      '1'           XXPRT1            1
     C                   EXCEPT    HEADE
     C                   ENDIF
      *
      *** BEG DEL *** 06/19/06 ****************************
     C********** XXPRT2    IFNE '1'
     C**********           OPEN QSYSPRT2
     C**********           MOVE '1'       XXPRT2  1
     C**********           EXCPTHEADS
     C**********           ENDIF
      *** END DEL *** 06/19/06 ****************************
      *
      *** CLEAR WORK FIELDS
      *
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
     C                   MOVE      *ZEROS        E2               30
     C                   MOVE      *ZEROS        E3               30
     C                   MOVE      *ZEROS        E4               30
     C                   MOVE      *ZEROS        S2               30
     C                   MOVE      *ZEROS        S3               30
     C                   MOVE      *ZEROS        S4               30
     C                   MOVE      *ZEROS        L2               30
     C                   MOVE      *ZEROS        L3               30
     C                   MOVE      *ZEROS        L4                3 0
     C                   MOVE      *ZEROS        XT
     C                   MOVEA     '00000000'    *IN(90)
     C                   MOVEA     '00'          *IN(98)
     C                   MOVEL     *BLANKS       XXHKEY
     C                   MOVEL     *BLANKS       XXHKY1
     C                   MOVEL     *BLANKS       NWKEY
     C                   MOVEL     *BLANKS       NWKY1
     C                   MOVEL     *BLANKS       XXHDT             8
     C                   MOVEL     *BLANKS       XXHTM             6
     C                   MOVEL     *BLANKS       XXHDSC          132
      *
      *
     C                   EXSR      PRCFIL
      *
      ***   LOOP TO NEXT SXFL RECORD
     C     FLKY          READE     PFL01                                  61
      *
     C                   ENDDO
      *
     C*
      *
      * End the program
     C     ENDIT         TAG
      *** BEG ADD *** 06/19/06 ****************************
     C                   MOVE      'Y'           QQINLR
     C                   EXSR      WRTLOG
      *** END ADD *** 06/19/06 ****************************
     C                   MOVE      *ON           *INLR
      *
      *===================* END OF MAINLINE CODE *====================*
      *
      *****************************************************************
      * * *     SUBROUTINE - PRCFIL - PROCESS FILES FROM IFS      * * *
      *****************************************************************
      *
     C     PRCFIL        BEGSR
      *
      *
      ***   FOR EACH RCD,
      ***     SUBROUTINE TO EXTRACT KEY VALUE FROM FILE NAME
      ***     EXTRACT KEY
      *
     C                   EXSR      NAMFLD
      *
      *** TAKE DATA FOUND, AND SETUP VARIABLES
      *
     C                   MOVE      *BLANKS       #$CLR
     C                   MOVE      *BLANKS       #$CLR1
     C     ' '           CHECKR    XXHKEY        XXHLKN            3 0            FIND LEN OF KEY
      ***
     C                   MOVE      *BLANKS       @@KEY
     C                   MOVE      *BLANKS       LLKEY
     C*** ADD 12/29/03 ************************
     C                   MOVEL     'SUMPOL'      X#KTP
     C*** END ADD 12/29/03 *************************
     C     XXHLKN        IFGT      9
     C*** INC. YEAR IN KEY
     C                   MOVE      '001'         #@CO#
     C                   MOVE      #$FYR         #@FYR
     C                   MOVE      #$CLNT        #@CLNT
     C                   MOVE      #$CARR        #@CARR
     C                   MOVEL     NWKEY         @@KEY            40
     C                   MOVE      #@CO#         LLCO#
     C                   MOVE      #$CLNT        LLCLNT
     C                   MOVE      #$CARR        LLCARR
     *** ADD 12/29/03 *********************************
     C                   MOVE      '001'         #@CO#1
     C                   MOVE      #$CLNT        #@CLN1
     C                   MOVE      #$CARR        #@CAR1
     *** END ADD 12/29/03 ****************************
     C                   ELSE
     C*** ADD 12/29/03 ************************
     C                   MOVEL     'SUMPL1'      X#KTP
     C*** END ADD 12/29/03 *************************
     C                   MOVE      '001'         #@CO#1
     C                   MOVE      #$CLN1        #@CLN1
     C                   MOVE      #$CAR1        #@CAR1
     C                   MOVE      #@CO#1        LLCO#
     C                   MOVE      #@CLN1        LLCLNT
     C                   MOVE      #@CAR1        LLCARR
     C                   MOVEL     NWKY1         @@KEY
     C                   ENDIF
     C*******************  MOVELNWKEY     @@KEY  40
     C*****************    MOVELXXHKEY    @@KEY  40
      *
      *** CONVERT DATE/TIME TO USEFUL VALUES
      *
      * DATE TO MMDDCCYY
     C*
     C                   MOVE      *ZEROS        SRDTI
     C************         MOVE XXHDT     SRDTI
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         @@CDAT            8            DT FOR API
      *
      * TIME AS IS 6A
     C                   MOVE      *ZEROS        @@CTIM
     C***********          MOVELXXHTM     @@CTIM  6
     C                   MOVEL     DSTIME        @@CTIM            6
      *
      * DESC AS IS 132 (AS COMMENT IN DARTS, OR MOREOBJECTS)
      *
     C                   MOVE      *BLANKS       @@DESC
     C                   MOVE      *BLANKS       @@DESC          132
     C*******************  MOVELXXHDSC    @@DESC132
      *
      ***        VALIDATE KEY IN SAME SUBROUTINE
      ***  VALIDATE KEY
     C                   CALL      'ISIV81R'                                    COULD GET FRM FIL
     C                   PARM                    LLKEY            40
     C                   PARM                    @@RTNC            1
      *
      ***        NOT VALID
     C     @@RTNC        IFNE      ' '
     C                   MOVEL     @@RTNC        @@VRCD            2
      *
      *** EXECUTE ERROR SUBROUTINE ***
      *
     C                   EXSR      ERROR1
      *
      * @@ERCD-COPY TO ERROR DIRECTORY, @@DLCD-DELETE FROM ORIGINAL DIRECTRY
      *
      *** FORMAT THE ERROR REPORT DETAIL LINE HERE ***
      *
      *
     C****       *INOA     IFEQ *ON
     C*                    EXCPTHEADE
     C*                    ENDIF
      *
     C*                    EXCPTERR1
      *
     C*** LOOP FOR THE ERRORS
     C*          *INOA     IFEQ *ON
     C*                    EXCPTHEADE
     C*                    EXCPTERR1
     C*                    ENDIF
      *
     C****                 MOVEL*BLANKS   $$ERCD
     C*                    MOVEL*BLANKS   $$ERDS
     C*                    MOVEL'VR'      $$ERCD  2
     C*                    MOVEL@VR       $$ERDS 80
     C****                 EXCPTERR2
      *
     C*          @@ERCD    IFNE *BLANK
     C*                    MOVEL*BLANKS   $$ERCD
     C*                    MOVEL*BLANKS   $$ERDS
     C*                    MOVEL@@ERCD    $$ERCD  2
     C*                    MOVEL@ER       $$ERDS 80
     C*                    EXCPTERR2
     C***                  ENDIF
     C*
      *
     C*          @@DLCD    IFNE *BLANK
     C*                    MOVEL*BLANKS   $$ERCD
     C*                    MOVEL*BLANKS   $$ERDS
     C*                    MOVEL@@DLCD    $$ERCD  2
     C*                    MOVEL@DL       $$ERDS 80
     C*                    EXCPTERR2
     C*                    ENDIF
     C*
      *
     C                   ELSE
      ***        VALID -
      ***             GET NEW NAME FOR THE FILE (QDLS/FOLDER BALANCE)
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
     C                   MOVE      *BLANKS       @@WK1
     C                   MOVE      *BLANKS       @@WK2
     C                   MOVE      *BLANKS       @@WK3
      *** Beg chg ***** 11/02/10 ***********************************************
     C*******    '\QDLS\'  CAT  @@IFLR:0  @@WK1  14
     C     '\'           CAT(P)    ROOT          RPATH            12
     C     RPATH         CAT(P)    '\':0         RPATH
     C     RPATH         CAT       @@IFLR:0      @@WK1            20
      *** End chg ***** 11/02/10 ***********************************************
     C     @@WK1         CAT       '\':0         @@WK2            21
     C     @@WK2         CAT       @@ISFL:0      @@WK3            33
     C*
      *
      *
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
     *** ADD 12/29/03 ******************************
     C     XXHLKN        IFGT      9
     C                   MOVEL     'SUMPL1'      X#KTP             8
     C                   MOVE      *BLANKS       @@KEY
     C                   MOVEL     NWKY1         @@KEY
     C                   EXSR      DRTRCD
     C                   ENDIF
     *** END ADD 12/29/03 **************************
      ***             MOVE OBJECT TO THE ARCHIVE FOLDER  (IFS)
      *
     C     I1ADIR        IFNE      *BLANKS
      *
     C*** ADD 02/27/03 *** C:02 *************************
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        $$DAT             6
     C                   MOVE      DSTIME        $$TIM             6
     C                   MOVEL     $$DAT         $$DTTM           12
     C                   MOVE      $$TIM         $$DTTM
     C*
     C     $$DTTM        CAT(P)    FLOBJN:0      $$NOBJ          132
     C*
      *
     C***  END ADD ***8 02/27/03 ********8
      *
     C                   CALL      'CPYOBJC'
     C                   PARM      I1IDIR        @@FDIR           80
     C                   PARM      FLOBJN        @@FOBJ          132
     C                   PARM      I1ADIR        @@TDIR           80
     C**** CHG 02/27/03 ** PARM FLOBJN    @@TOBJ132
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
      *
     C                   ENDIF                                                  IF FB OK  (FBCD)
     C                   ENDIF                                                  IF QDLS OK (QDCD)
      ***
     C                   ELSE
     C*** ERROR ON DI RECORD... COULDN'T CONTINUE... ERROR RPT???
      *
     C                   ENDIF
     C                   ENDIF
     C*** Error or Success Report here
      *
     C     @@DICD        IFNE      *BLANK
     C     @@ERCD        ORNE      *BLANK
     C     @@QDCD        ORNE      *BLANK
     C     @@FBCD        ORNE      *BLANK
     C     @@ARCD        ORNE      *BLANK
     C     @@DLCD        ORNE      *BLANK
     C     @@VRCD        ORNE      *BLANK
      ***   ERROR REPORT
      *
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEADE
     C                   ENDIF
      *
     C                   EXCEPT    ERR1
      *
     C*** LOOP FOR THE ERRORS
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEADE
     C                   EXCEPT    ERR1
     C                   ENDIF
      *
     C     @@DICD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@DICD        $$ERCD            2
     C*** BEGIN ADD *** 05/30/08 **************************************
     C     @@DICD        IFEQ      'S'
     C*** BEGIN ADD *** 01/28/10 **************************************
     C                   CALL      'RTVBRND'                            99
     C                   PARM      'D'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C*
     C                   MOVEL     #BRND1        @RO              80
     C*** END ADD  **** 01/28/10 **************************************
     C                   MOVEL     @RO           $$ERDS
     C                   ELSE
     C*** END   ADD *** 05/30/08 **************************************
     C                   MOVEL     @DI           $$ERDS           80
     C*** BEGIN ADD *** 05/30/08 **************************************
     C                   ENDIF
     C*** END   ADD *** 05/30/08 **************************************
      *** BEG ADD *** 06/19/06 ****************************
     C                   MOVE      'U'           QQSTS
     C                   EXSR      WRTLOG
      *** END ADD *** 06/19/06 ****************************
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C*
     C     @@ERCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@ERCD        $$ERCD            2
     C                   MOVEL     @ER           $$ERDS           80
      *** BEG ADD *** 06/19/06 ****************************
     C                   MOVE      'U'           QQSTS
     C                   EXSR      WRTLOG
      *** END ADD *** 06/19/06 ****************************
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C     @@QDCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@QDCD        $$ERCD            2
     C                   MOVEL     @QD           $$ERDS           80
      *** BEG ADD *** 06/19/06 ****************************
     C                   MOVE      'U'           QQSTS
     C                   EXSR      WRTLOG
      *** END ADD *** 06/19/06 ****************************
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C     @@FBCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@FBCD        $$ERCD            2
     C*** BEGIN ADD *** 05/30/08 **************************************
     C     @@FBCD        IFEQ      'S'
     C*** BEGIN ADD *** 01/28/10 **************************************
     C                   CALL      'RTVBRND'                            99
     C                   PARM      'D'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C*
     C                   MOVEL     #BRND1        @RO              80
     C*** END ADD  **** 01/28/10 **************************************
     C                   MOVEL     @RO           $$ERDS
     C                   ELSE
     C*** END   ADD *** 05/30/08 **************************************
     C                   MOVEL     @FB           $$ERDS           80
     C*** BEGIN ADD *** 05/30/08 **************************************
     C                   ENDIF
     C*** END   ADD *** 05/30/08 **************************************
      *** BEG ADD *** 06/19/06 ****************************
     C                   MOVE      'U'           QQSTS
     C                   EXSR      WRTLOG
      *** END ADD *** 06/19/06 ****************************
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
      *
     C*
     C     @@ARCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@ARCD        $$ERCD            2
     C                   MOVEL     @AR           $$ERDS           80
      *** BEG ADD *** 06/19/06 ****************************
     C                   MOVE      'U'           QQSTS
     C                   EXSR      WRTLOG
      *** END ADD *** 06/19/06 ****************************
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C*
     C     @@DLCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@DLCD        $$ERCD            2
     C                   MOVEL     @DL           $$ERDS           80
      *** BEG ADD *** 06/19/06 ****************************
     C                   MOVE      'U'           QQSTS
     C                   EXSR      WRTLOG
      *** END ADD *** 06/19/06 ****************************
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C*
     C     @@VRCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@VRCD        $$ERCD            2
     C                   MOVEL     @VR           $$ERDS           80
      *** BEG ADD *** 06/19/06 ****************************
     C                   MOVE      'U'           QQSTS
     C                   EXSR      WRTLOG
      *** END ADD *** 06/19/06 ****************************
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C                   EXCEPT    ERR3
      *
     C                   ELSE
      *
      *
      ***   WRITE SUCCESS REPORT
      *** BEG DEL *** 06/19/06 ****************************
     C*********            EXCPTSUCCS1
      *** END DEL *** 06/19/06 ****************************
      *** BEG ADD *** 06/19/06 ****************************
     C                   MOVE      'S'           QQSTS
     C                   EXSR      WRTLOG
      *** END ADD *** 06/19/06 ****************************
     C                   ENDIF
     C                   ENDSR
      *
      *
      *****************************************************************
      * * *     SUBROUTINE - NAMFLD - BREAK FILE NAME TO FIELDS   * * *
      *****************************************************************
      *
     C     NAMFLD        BEGSR
      *
      *** CLEAR WORK FIELDS
      *
     C                   MOVEL     FLOBJN        @@FULL          132
      *
      *** EXTRACT THE KEY, DATE, TIME, DESCRIPTION FROM THE FULL OBJ NAME
      *                    FND IS AN ARRAY AND CONTAINS ALL OCCURANCES OF
      *                    " "
      *
     C     ' '           SCAN      @@FULL:1      FND                      90
     C     '.'           SCAN      @@FULL:1      XT                3 0    99      FILE EXTN STRT
     C*** 09/24/03 ** 02 **** ADDED TO AVOID BOMB ON NO EXTENTION FND
     C     XT            IFLE      0
     C*** ERROR *** NO EXTENTION
     C** MAKE IT A DUMMY... .TXT
     C                   GOTO      ENDNAM
     C                   ENDIF
     C***
     C*** END ADD 09/24/03 ***********************
     C*** GET EXTENTION TO FILE
     C     4             SUBST     @@FULL:XT     @@EXTN            4              FILE EXTNTN
     C                   MOVEL     @@EXTN        WK1               1
     C     WK1           IFNE      '.'
     C                   MOVEL     '.'           @@EXTN
     C                   MOVE      'OBJ'         @@EXTN
     C                   ENDIF
     C*
     C     FND(1)        SUB       1             E1                3 0
     C     E1            IFGT      0
      *  EXTRACT THE KEY
     C     E1            SUBST     @@FULL:1      XXHKEY           40    91      HOLD KEY
     C     E1            SUBST     @@FULL:1      XXHKY1           40    91      HOLD KEY
      *  EXTRACT THE DATE
     C*********  FND,1     ADD  1         S2      30       START OF 2ND PART
     C***        FND,2     SUB  S2        L2      30       LENGTH OF 2ND PRT
     C***        FND,2     SUB  1         E2      30       END OF 2ND PRT
     C*
     C********   L2        SUBST@@FULL:S2 XXHDT   6    92  HOLD DATE
      *
      *  EXTRACT THE TIME
     C********   FND,2     ADD  1         S3      30       START OF 3RD PART
     C**         FND,3     SUB  S3        L3      30       LENGTH OF 3RD PRT
     C**         FND,3     SUB  1         E3      30       END OF 3RD PRT
     C*
     C********   L3        SUBST@@FULL:S3 XXHTM   6    93  HOLD TIME
      *
      *
      *  EXTRACT THE DESCRIPTION
     C     *IN99         IFEQ      *OFF                                         OF @@FULL OR NO
     C************         Z-ADD133       FND,4            EXTNTN FOUND..
     C                   ELSE
     C************         Z-ADDXT        FND,4            ONLY ONE '.'
     C                   ENDIF                                                                   NDI
     C********   FND,3     ADD  1         S4      30       START OF 3RD PART
     C********   FND,4     SUB  S4        L4      30       LENGTH OF 3RD PRT
     C********   FND,4     SUB  1         E4      30       END OF 3RD PRT
     C*
     C********   L4        SUBST@@FULL:S4 XXHDSC132    94  HOLD DESCRIPTION
      *
      *
     C                   ENDIF
      *
      *** SUBSTRING ERROR
      ***  IF 91,2,3, OR 4 IS ON, THEN AN ERROR OCCURED ON A SUBSTR... NO
      ***  VALUE WAS CAPTURED
      *
     C     ENDNAM        ENDSR
      *
      *
      *****************************************************************
      * * *     SUBROUTINE - ERROR1 - ERROR SUBROUTINE 1          * * *
      *****************************************************************
      *
     C     ERROR1        BEGSR
      *
      ***           MOVE TO ERROR DIRECTORY
      *
     C     I1EDIR        IFNE      *BLANKS
     C*** ADD 02/27/03 *** C:02 *************************
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        $$DAT             6
     C                   MOVE      DSTIME        $$TIM             6
     C                   MOVEL     $$DAT         $$DTTM           12
     C                   MOVE      $$TIM         $$DTTM
     C*
     C     $$DTTM        CAT(P)    FLOBJN:0      $$NOBJ          132
     C*
      *
     C***  END ADD ***8 02/27/03 ********8
      *
     C                   CALL      'CPYOBJC'
     C                   PARM      I1IDIR        @@FDIR           80
     C                   PARM      FLOBJN        @@FOBJ          132
     C                   PARM      I1EDIR        @@TDIR           80
     C**** CHG 02/27/03 ** PARM FLOBJN    @@TOBJ132
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
      *
      *****************************************************************
      * * *     SUBROUTINE - DRTRCD - CREATE DART RECORD FOR IMPT * * *
      *****************************************************************
      *
     C     DRTRCD        BEGSR
      *
     C*** WRITE REMARK RECORD
     C*
     C*  *** PROGRAM NAME TO USE FOR PNAM - KEY STRUCTURE
     C*** DEL 12/29/03 **  MOVEL'SUMPOL  '##PNAM 10
     C*** ADD 12/29/03 *******************************
     C                   MOVEL     X#KTP         ##PNAM           10
     C*** END ADD 12/29/03 ***********************
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
     C     I1IDXT        IFEQ      'M'
     C                   MOVEL     *BLANKS       Q#DCID
     C                   MOVEL     *BLANKS       Q#IMGF
     C                   ELSE
     C                   MOVEL(P)  I1IMGF        Q#IMGF            1
     C                   MOVEL(P)  @@IDOC        Q#DCID           12
     C                   ENDIF
     C                   MOVEL(P)  ##PNAM        Q#PNAM           10
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
     C*
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
      *
     C                   ENDSR
      *
      *****************************************************************
      * * *     SUBROUTINE - MORCD - CREATE MORE OBJ RECORD       * * *
      *****************************************************************
      *
     C     MORCD         BEGSR
      *
      *** 1ST CREATE THE MAJOR REMARK RECORD
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
      *
      *** BEG ADD *** 06/19/06 ****************************
      *
      *****************************************************************
      * * *     SUBROUTINE - MORCD - CREATE MORE OBJ RECORD       * * *
      *****************************************************************
      *
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
     C                   MOVEL(P)  I1ADIR        QQTDIR                         ADDMJF
     C                   ENDIF
      *
     C     QQSTS         IFEQ      'U'
     C                   MOVE      I1EDIR        QQEDIR
     C                   MOVE(P)   I1EDIR        QQTDIR                         ADDMJF
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
     C                   PARM                    QQTDIR           80            CHGMJF
     C                   PARM      $$NOBJ        QQTFIL          132            CHGMJF
     C                   PARM                    QQERCD            2
     C                   PARM                    QQERDE           80
     C                   PARM                    QQSTS             1
     C                   PARM      Q1PGM         QQPGM            10
     C                   PARM                    QQINLR            1
      *
     C                   ENDSR
      *
      *** END ADD *** 06/19/06 ****************************
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
      *** BEG DEL *** 06/19/06 ****************************
     O*QSYSPRT2E  106 ********* HEADS
     O*******                           111 'OBJECT IMPORT PROCESSED '
     O*******                  DSDATEY   12
     O*******                  DSTIME   196 '  :  :  '
     O******* E  3             HEADS
     O*******                            14 'FROM DIR:'
     O*******                  I1IDIR    95
     O*******
     O*******                           112 'ARCHIVE DIR:'
     O*******                  I1ADIR   193
     O*******
     O******* E  1             HEADS
     O*******                            10 'FILE NAME'
     O*******                           144 'TO FILE'
     O******* E  1             SUCCS1
     O*******                  FLOBJN   133
     O*******                  XXTDIR   198
      *** END DEL *** 06/19/06 ****************************
