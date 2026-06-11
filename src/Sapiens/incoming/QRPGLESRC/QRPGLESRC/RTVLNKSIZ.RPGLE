    h* (C) Copyright Fiserv Insurance Solutions, Inc. 2008
    H*
    H/TITLE RTVLNKSIZ - Retrieve Link Size
    H**********************************************************************
    H*                                                                    *
    H* Called From: unknown                                               *
    H* Calls to   : none                                                  *
    H*                                                                    *
    H**********************************************************************
    H* Created on 08/06/2008 by c:62 for Project 143IAD                   *
    H**********************************************************************
    H* Revisions:                                                         *
    H*                                                                    *
    H**********************************************************************
     D stat            PR            10I 0 ExtProc('stat')
     D   path                          *   value options(*string)
     D   buf                           *   value

     D p_statds        S               *

     D statds          DS                  BASED(p_statds)
     D  st_mode                      10U 0
     D  st_ino                       10U 0
     D  st_nlink                      5U 0
     D  st_pad                        2A
     D  st_uid                       10U 0
     D  st_gid                       10U 0
     D  st_size                      10I 0
     D  st_atime                     10I 0
     D  st_mtime                     10I 0
     D  st_ctime                     10I 0
     D  st_dev                       10U 0
     D  st_blksize                   10U 0
     D  st_allocsize                 10U 0
     D  st_objtype                   12A
     D  st_codepage                   5U 0
     D  st_reserved1                 62A
     D  st_ino_gen_id                10U 0

     D MyStat          S                   like(statds)

     D @@path          S            100A
     D @@Size          S             10I 0
     D @@rtncd         S              1A
    C*****************************************************************
    C*  Parameter Lists
    C*****************************************************************
     C     *entry        plist
     C                   parm                    @@path
     C                   parm                    @@size
     C                   parm                    @@rtncd
     C*
      * get stat info into "MyStat":
     c                   if        stat(%trim(@@path):%addr(mystat)) < 0
     c                   eval      @@rtncd='F'
     c                   else

      * move structure to overlay the "mystat" info:
     c                   eval      p_statds = %addr(mystat)

      * read the file's size into MySize:
     c                   eval      @@Size = st_size
     c                   endif
     C*
     C                   return
