     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      ************************************************************************/
      ************************************************************************/
      * SCI110R - Object Import cLEANUP processor                            */
      *                                                                      */
      * Description: Runs ONCE DAILY OR WEEKLY (WHENEVER) TO CLEAN UP ARCHIV */
      *              OBJECTS for
      *                                                                      */
      * Created:     12/26/06                                                */
      * Programmer:  C:002                                                   */
      * Project:     OBJIMPT1                                               */
      **************************************************************   *******/
      * Revisions:                                                           */
      *                                                                      */
      * -------------------------------------------------------------------- */
      *  Note: Please add comments to the top of revisions.                  */
      * -------------------------------------------------------------------- */
      ***************************************************************  *******/
      *
     FSMI1L1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D DBFDS           DS
     D  DBRRN                397    400B 0
      *
      * RESET POINTER TO DB FILE (SMI1L?) TO BOF

     C     *LOVAL        SETLL     PI101
     C                   READ      PI101                                  60
      *
      * READ EACH RECORD (LOOP) IN SMI1L?
     C     *IN60         DOWEQ     *OFF
      *
     C     I1ADIR        CABEQ     *BLANKS       BYPS1
     C*
      *   FOR EACH RCD READ
      *   CLEAR SXFILES (DB FILE LISTS FILES IN AN IFS DIRECTORY)
      *   BUILD SXFILES (IFS COMMAND) FOR DIRECTORY FROM VAR. I1IDIR
     C                   CALL      'SCI110C1'
     C                   PARM                    I1ADIR
      *
      *   CALL TO PROGRAM SCI110R1 (PROCESS FILES IN THE SXFILES LIST)
      *     OPEN SXFILES (MANUAL OPEN)
     C*     OPEN QSYSPRT/1, ONLY IF SXFILES HAS RCDS FOR REPORTING
      *     ENDS WITH *LR ON
      *                               PASS RRN FOR SMI1P?
      *
     C                   CALL      'SCI110R1'
     C                   PARM      DBRRN         @@RRN             9 0
     C                   PARM                    RTNCD             2
      *
     C     BYPS1         TAG
      *
      *  LOOP TO GET NEXT RCD, UNTIL EOF
     C                   READ      PI101                                  60
      *
     C                   ENDDO
      *
      *
      * End the program
     C                   MOVE      *ON           *INLR
