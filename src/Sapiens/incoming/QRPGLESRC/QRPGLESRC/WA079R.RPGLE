     H* (c) 2010 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
      *=========================================================================
     H/TITLE - Program to Update WXICP with Job specific subfolder.            *
      *                                                                        *
      *  DESCRIPTION: This Program will update WXICP file with job specific    *
      *  subfolder that will be passed as parameter to this program by calli-  *
      *  ng program.                                                           *
      *                                                                        *
      *   CALLED BY: W7051C1,W7413C, W8043C                                    *
      *                                                                        *
      *   CALLS TO :                                                           *
      *                                                                        *
      *  CREATED: 04/30/10    C:255 PROJECT WC1866                             *
      *=========================================================================
      **************************************************************************
      * REVISIONS:
      *                                                                        *
      * 03/08/11 C:189 Support: WA132 SEQ76080                                 *
      *                The incorrect schedule of locations is attaching        *
      *                on running the mid term endoresment                     *
      *                                                                        *
      *                                                                        *
      **************************************************************************
     FWXICP     UF   E           k disk
      *=========================================================================
     D@@strpos         S              5  0
     D@@length         S              5  0
     D*** beg chg *** 03/08/11 ****************************************
     D************@@flrnam         S             10A
     D@@flrnam         S             20A
     DXXflrnam         S             20A
     DXXPATH           S             20A
     D*** end chg *** 03/08/11 ****************************************
     C     *loval        setll     WXICP
     C                   Read      PIC01
     C                   Dow       not %Eof(WXICP)
     C*
     C                   Eval      @@strpos= %Scan(',':ICSTMT) +1
     C                   Eval      @@length= %Scan(',':ICSTMT:@@strpos) -
     C                                              @@strpos
     c                   Eval      @@flrnam= %Subst(ICSTMT:@@strpos:@@length)
     C*
     C*** beg chg *** 03/08/11 ****************************************
     C                   EVAL      xxflrnam =  %TRIM(@@flrnam)
     C
     C                   Call      'SA030R'
     C                   Parm                    suffix            3
     C                   Parm                    pflr             80
     C
     C                   If        pflr <> *blanks
     C                   Eval      XXPATH  =  'PDS/'+ SUFFIX + '/AMTEMP'
     C                   ELSE
     C                   Eval      XXPATH  =  'AMTEMP'
     C                   Endif
     C
     C                   IF        XXFLRNAM = XXPATH  OR  XXFLRNAM = 'AMTEMP'
     C*****************  If        @@flrnam = 'AMTEMP'
     C*** end chg *** 03/08/11 ****************************************
     C                   Eval      ICSTMT=  %Replace(%trim(##subflr):ICSTMT:  +
     C                                               @@strpos : @@length)
     C*
     C                   UPDATE    PIC01
     C                   EndIf
     C*
     C                   Read      PIC01
     C                   EndDo
     C*
     C*End of Processing
     C                   Eval      *Inlr=*On
      *****************************************************************
      *  INITIALIZATION                ** SUBROUTINE **   S R 1 S T   *
      *****************************************************************
     C     *Inzsr        Begsr
    C*
     C     *Entry        Plist
     C                   Parm                    ##SUBFLR         80
     C                   Parm                    ##RTN             1
     C*
     C                   Endsr
