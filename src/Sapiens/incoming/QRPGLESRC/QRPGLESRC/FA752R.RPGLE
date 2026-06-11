     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*
    H/TITLE FA752R - Line Parser
    Hdebug(*yes) option(*srcstmt:*nodebugio)
     H dftactgrp(*no) actgrp(*caller)
    H**********************************************************************
    H*                                                                    *
    H* Called From: F0340R  - EOMB Print                                  *
    H*              W7002R  - Micr Check Print                            *
    H* Calls to   : None                                                  *
    H*                                                                    *
    H**********************************************************************
    H* Created on 10/22/2007 by c:62 for Project 139MIF                   *
    H**********************************************************************
    H* Revisions:                                                         *
    H*                                                                    *
    H**********************************************************************
     D eInput          s            255A
     D eOutput1        s            126A
     D eOutput2        s            126A
     D eOutput3        s            126A
     D input           s            255A
     D output          s            126A
     D inputA          s              1A   dim(255)
     D x               s              3s 0
     D y               s              3s 0
    C*****************************************************************
    C*  Parameter Lists
    C*****************************************************************
     C     *entry        plist
     C                   parm                    eInput
     C                   parm                    eOutput1
     C                   parm                    eOutput2
     C                   parm                    eOutput3
     C*
     C                   movea     eInput        inputA
     C                   eval      input=eInput
     C*
     C                   z-add     1             z                 3 0
     C     1             do        3             y                 3 0
     C                   eval      x=127
     C                   clear                   output
     C                   dow       x>0
     c                   if        inputA(x) = ' '
     C                   eval      x=x-1
     C                   eval      output=%subst(input:1:x)
     C                   eval      x=x+1
     C                   movea(p)  inputA(x)     input
     c                   movea(p)  input         inputA
     C                   leave
     C                   endif
     C                   eval      x=x-1
     C                   enddo
     c*
     C                   select
     c                   when      y=1
     C                   eval      eOutput1=%trim(output)
     c                   when      y=2
     C                   eval      eOutput2=%trim(output)
     c                   when      y=3
     C                   eval      eOutput3=%trim(output)
     c                   endsl
     C*
     C                   enddo
     C*
     C                   return
