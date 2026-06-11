     *===============================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2005          *
     *===============================================================*
     * TITLE SC718R - REMARKS PRINT - CALL PROGRAM                   *
     *===============================================================*
     * PROGRAM DESCRIPTION:  This program is called from Visual      *
     *         DARTS to generate a listing of all remarks for a      *
     *         given key.                                            *
     *                                                               *
     * CREATED   : 12/05/99                                          *
     * PROGRAMMER: C:113                                             *
     * PROJECT   : DR081                                             *
     *                                                               *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 07/11/05 c:62 project DR193                                   *
     *               add "output type" parameter.                    *
     *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
     * ------------------------------------------------------------- *
     *===============================================================*
     *===============================================================*
      * Definition Specifications
     *===============================================================*
     D                 DS
     D dummy                         40
     D  dum40                         1    overlay(dummy:40)

     D qqkey           s             40
     D qqmode          s              1
     D qqque           s             10
     C*** begin add *** 07/11/05 *******************************************
     D qqoutt          s              1
     D qqfrdt          s              7
     D qqtodt          s              7
     C*** end   add *** 07/11/05 *******************************************
     *===============================================================*
     * PLIST Definitions                                             *
     *===============================================================*
     C     *entry        plist
     C                   parm                    qqkey
     C                   parm                    qqque
     C                   parm                    qqmode
     C*** begin add *** 07/11/05 *******************************************
     C                   parm                    qqoutt
     C                   parm                    qqfrdt
     C                   parm                    qqtodt
     C*** end   add *** 07/11/05 *******************************************
     *===============================================================*
     * Main                                                          *
     *===============================================================*
      ** Check parm for last char blank (parms longer than 30 char)
     C                   move(p)   qqkey         dummy
     C                   if        dum40 = ' '
     C                   move      '¢'           qqkey
     C                   endif

     C                   call      'SC718C'
     C                   parm                    qqkey
     C                   parm                    qqque
     C                   parm                    qqmode
     C*** begin add *** 07/11/05 *******************************************
     C                   parm                    qqoutt
     C                   parm                    qqfrdt
     C                   parm                    qqtodt
     C*** end   add *** 07/11/05 *******************************************

     C                   eval      *inlr = *on
