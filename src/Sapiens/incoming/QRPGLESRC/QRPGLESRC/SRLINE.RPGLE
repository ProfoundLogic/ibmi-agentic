     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     *===============================================================*
    H/TITLE SRLINE - Return line broken at word boundry
     *===============================================================*
      *  Program parse line of text starting from position specified
      *  (qqlength) and proceeding to the left seaching for a break in
      *  the text (a blank character).  Returns first half of text line
      *  (qqline) and remaining text from line (qqremain).  Default is to
      *  return line (qqline) of specified length (qqlength) in the event
      *  line does not contain any blanks.
      *
      *  GIVEN:  -line of text
      *          -desired line length (maximum of 256 characters)
      *
      *  RETURN: -line truncated to nearest complete word.
      *          -remaining text from line
      *
     *===============================================================*
     * Revisions:                                                    *
     *                                                               *
     * 11/15/06 C:61  project 139WCI                                 *
     *                Created.
     *                                                               *
     *===============================================================*

     *=========================================================================
     *  Definition Specs
     *=========================================================================

     d xxline          s            256
     d I1              s              3  0


     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     * Entry

     c     *entry        plist
     c                   parm                    qqline          256
     c                   parm                    qqlength          3 0
     c                   parm                    qqremain        256


     *=========================================================================
     *  Main
     *=========================================================================

     c                   eval      xxline = qqline

     c                   if        qqlength >= 256
     c                   eval      qqlength = 255
     c                   endif

     c                   eval      i1 = qqlength

     c                   dow       (i1 > 0)
     c                             and (%subst(xxline:i1:1) <> *blank)
     c                   eval      i1 = i1 - 1
     c                   enddo

     c                   if        i1 = 0
     c                   eval      qqline = %subst(xxline:1:qqlength)
     c                   eval      i1 = qqlength + 1
     c                   eval      qqremain = %subst(xxline:i1)

     c                   else

     c                   eval      qqline = %subst(xxline:1:i1)
     c                   eval      i1 = i1 + 1
     c                   eval      qqremain = %subst(xxline:i1)
     c                   endif

     c                   eval      *inlr = *on
    

