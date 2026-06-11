     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      ************************************************************************/
      ************************************************************************/
      * SCI105R - Object Import control processor                            */
      *                                                                      */
      * Description: Runs as Auto-Start job, processes imports of            */
      *              OBJECTS for Darts
      *                                                                      */
      * Created:     04/16/01                                                */
      * Programmer:  C:002                                                   */
      * Project:     115AAC                                                  */
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
      *** WAIT FOR X MIN. THEN PROCESS
      *
      *
      *
      * RESET POINTER TO DB FILE (SMI1L?) TO BOF

     C     *LOVAL        SETLL     PI101
     C                   READ      PI101                                  60
      *
      * READ EACH RECORD (LOOP) IN SMI1L?
     C     *IN60         DOWEQ     *OFF
      *
      *   FOR EACH RCD READ
      *   CLEAR SXFILES (DB FILE LISTS FILES IN AN IFS DIRECTORY)
      *   BUILD SXFILES (IFS COMMAND) FOR DIRECTORY FROM VAR. I1IDIR
     C                   CALL      'SCI105C1'
     C                   PARM                    I1IDIR
      *
      *   CALL TO PROGRAM I1KPGM (PROCESS FILES IN THE SXFILES LIST)
      *     OPEN SXFILES (MANUAL OPEN)
     C*     OPEN QSYSPRT/1, ONLY IF SXFILES HAS RCDS FOR REPORTING
      *     ENDS WITH *LR ON
      *                               PASS RRN FOR SMI1P?
      *
     C                   CALL      I1KPGM
     C                   PARM      DBRRN         @@RRN             9 0
     C                   PARM                    RTNCD             2
      *
      *
      *  LOOP TO GET NEXT RCD, UNTIL EOF
     C                   READ      PI101                                  60
      *
     C                   ENDDO
      *
      *  LOOP TO BEGIN OF PGM (WAIT 5 MIN)
      *
      * End the program
     C                   MOVE      *ON           *INLR
