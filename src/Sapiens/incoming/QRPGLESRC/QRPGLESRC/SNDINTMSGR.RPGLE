     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp(*CALLER)
     H/TITLE SNDINTMSGR - Send Interactive Message
      *====================================================================
      * Program Description:  This program will send a message to
      *  interactive jobs on a subsystem.
      *
      * Called From: SNDINTMSGC
      *
      *====================================================================
      * Revisions:
      *
      * 03/07/08 C:61  PROJECT 132WKL
      *                Created to send interactive users of subsystem
      *                (QINTER132) a message indicating BATCH processing
      *                is about to begin.
      *
      *====================================================================

      *--------------------------------------------------------------------
      * File Specifications
      *--------------------------------------------------------------------
      *
     Fsxtmp     if   e             disk
      *
      *
     d xxinter         s              5a
     d xxactive        s              6a
     d xxsession       s             10a
     d xxCMD           s            600a
     d xxLENGTH        s             15p 5

      *--------------------------------------------------------------------
      * PLIST definitions
      *--------------------------------------------------------------------
      ** Entry parameters
     C     *entry        plist
     C                   parm                    @@subsys         10
     C                   parm                    @@msg           512


      *====================================================================
      * Main
      *====================================================================

     c                   read      ptm01

     c                   dow       not %eof

     c                   eval      xxinter   = %subst(tmrec:39:5)
     c                   eval      xxactive  = %subst(tmrec:49:6)
     c                   eval      xxsession = %subst(tmrec:4:10)

     c                   if        (xxinter = 'INTER')
     c                             and (xxactive = 'ACTIVE')
     c                   eval      xxCMD = 'SNDBRKMSG MSG(' + ''''
     c                             + @@MSG + ''')'
     c                             + ' TOMSGQ(' + xxsession + ')'
     c                   eval      xxLENGTH = 600

     c                   call      'QCMDEXC'
     c                   parm                    xxCMD
     c                   parm                    xxLENGTH

     c                   endif

     c                   read      ptm01
     c                   enddo

     C                   move      *on           *inlr

