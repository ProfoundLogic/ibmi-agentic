     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      ************************************************************************/
      ************************************************************************/
      * IS811R  - LOOSE TIFF COPY FROM IFS TO QDLS                           */
      *                                                                      */
      * Description: CALLED FROM IS811C                                      */
      *                                                                      */
      * Created:                                                             */
      * Programmer:                                                          */
      * Project:                                                             */
      **************************************************************   *******/
      * Revisions:                                                           */
      *
      * -------------------------------------------------------------------- */
      *  Note: Please add comments to the top of revisions.                  */
      * -------------------------------------------------------------------- */
      ***************************************************************  *******/
      *
     FSXFLP     IF   E           K DISK

     D FND             S              3  0 DIM(132)                             FOUND ARRAY
     D R2              S              2    DIM(20)                              RTN CODES (2)ERR
     D R3              S              1    DIM(20)                              RTN CODES (3)SUCC
      *
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
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
     D @QD             C                   CONST('Error copying file t-
     D                                     o QLDS file system. -
     D                                     File not processed. -
     D                                                         ')
     i*

     C*
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
      *
     C     *ENTRY        PLIST
     C                   PARM                    @FFOLD           80
     C                   PARM                    @TFOLD           80
     C                   PARM                    RTNCD             1
      **
      *
      *
      *
     C*
     C                   MOVEL     '*STMF   '    @@TYPE           12
     C*
     C                   TIME                    DSDTTM
     C*
      *
     C     FLKY          CHAIN     PFL01                              61
     C     *IN61         IFEQ      *ON
     C                   MOVE      '2'           RTNCD
     C                   GOTO      ENDIT
     C                   ENDIF
      *
     C     *IN61         DOWEQ     *OFF
      *
      *
      *** CLEAR WORK FIELDS
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
      ***     1) GET NEW UNIQUE NAME FOR "TO" OBJECT NAME
     *** USE A COUNTER DATAAREA TO GET NEXT NUMBER CAT WITH LETTER FOR NAME
     ****
     C                   CALL      'IS811R1'
     C                   PARM      *BLANKS       QQDCID           12
     C                   PARM      *BLANKS       XRFLR            63
     C                   PARM      *BLANKS       FFRTC             1
     C*
      ***     2) COPY OBJECT FROM IFS TO QDLS WITH NEW NAME
      *** TAKE DATA FOUND, AND SETUP VARIABLES
      *
      ***             GET NEW NAME FOR THE FILE (QDLS/FOLDER BALANCE)
      *
      *
     C                   CALL      'CPYOBJC'
     C                   PARM      @FFOLD        @@FDIR           80
     C                   PARM      FLOBJN        @@FOBJ          132
     C                   PARM      @TFOLD        @@TDIR           80
     C                   PARM      QQDCID        @@TOBJ          132
     C                   PARM      '  '          @@RTN2            2
     C*
      *
     C     @@RTN2        IFNE      *BLANKS
     C                   MOVEL     @@RTN2        @@QDCD            2
     C                   ELSE
     C                   MOVEL     *BLANKS       @@QDCD
     C                   ENDIF
      *
     C     @@QDCD        IFEQ      *BLANKS
      *
      *
      *** ONLY DELETE IF THE COPY ABOVE WAS SUCCESSFUL (TO ERROR DIRECTORY)
      ***     3) DELETE OLD OBJECT FROM IFS
      *
     C                   CALL      'DELOBJC'
     C                   PARM      @FFOLD        @@FDIR           80
     C                   PARM      FLOBJN        @@FOBJ          132
     C                   PARM      '  '          @@RTN2            2
      *
     C     @@RTN2        IFNE      *BLANKS
     C                   MOVEL     @@RTN2        @@DLCD            2
     C                   ELSE
     C                   MOVEL     *BLANKS       @@DLCD
     C                   ENDIF
      *
     C                   ENDIF                                                  @@QDCD
      *
      *
     C                   ENDSR
      *
      *
      *
