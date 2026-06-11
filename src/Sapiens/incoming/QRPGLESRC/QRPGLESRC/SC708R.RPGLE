     *===============================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2003          *
     *===============================================================*
     * TITLE SC708R - Remarks Print - call program                   *
     *===============================================================*
     * Program Description:  This program is called from Visual      *
     *         DARTS to generate a listing of all remarks for a      *
     *         given key.                                            *
     *                                                               *
     *===============================================================*
     * Created on 06/27/2003 by c:62 for Project                     *
     *===============================================================*
     * Revisions:                                                    *
     *                                                               *
     *                                                               *
     *===============================================================*
     D                 DS
     D dummy                         40
     D  dum40                         1    overlay(dummy:40)

     D qqkey           s             40
     D qqmode          s              1
     D qqque           s             10
     D qqfdat          s              7
     D qqtdat          s              7
     D qqotyp          s              1
     D qqqry1          s            478
     *===============================================================*
     * PLIST Definitions                                             *
     *===============================================================*
     C     *entry        plist
     C                   parm                    qqkey
     C                   parm                    qqque
     C                   parm                    qqmode
     C                   parm                    qqfdat
     C                   parm                    qqtdat
     C                   parm                    qqotyp
     C                   parm                    qqqry1
     *===============================================================*
     * Main                                                          *
     *===============================================================*
      ** Check parm for last char blank (parms longer than 30 char)
     C                   move(p)   qqkey         dummy
     C                   if        dum40 = ' '
     C                   move      '¢'           qqkey
     C                   endif

     C                   call      'SC708C'
     C                   parm                    qqkey
     C                   parm                    qqque
     C                   parm                    qqmode
     C                   parm                    qqfdat
     C                   parm                    qqtdat
     C                   parm                    qqotyp
     C                   parm                    qqqry1

     C                   eval      *inlr = *on
