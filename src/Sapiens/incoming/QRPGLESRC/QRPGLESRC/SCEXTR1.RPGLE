     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*********************************************************************
     H/title SCEXTR1 - DARTS Extract - Primary
     H*********************************************************************
     H*                                                                   *
     H*  Created: 10/17/11   C:190   Project: 139WQI                      *
     H*********************************************************************
     H*  Revisions:                                                       *
     H*
     H*  12/23/19  C:180  Project:133IRJ                                  *
     H*                   Remove keycode parm.
     H*                                                                   *
     H*********************************************************************

     Fwxgsl     if   e           k disk

     D/COPY sprsecure

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@FrDt          s              8
     D @@ToDt          s              8
     D @@RunType       s              1
     D @@KeyCode       s             12
     d inKeyCode       s             12a
     d inTerm          s              1
     d LOGMSG          c                   CONST('Key code not valid. Process-
     d                                     terminated.')

     DQp0zLprintf      pr            10I 0 extProc('Qp0zLprintf')
     D szOutputStg                     *   value options(*string)
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
     C     *entry        plist
     C                   parm                    @@FrDt
     C                   parm                    @@ToDt
     C                   parm                    @@RunType
      *** Begin Delete *** 12/23/19 ***************************************
     C***                parm                    @@KeyCode
    C*
    C*================================================================
     c**                 eval      inKeyCode = @@KeyCode
     c**                 eval      inTerm = 'T'
     c**                 movel     logmsg        inmsg            50
     c**                 if        Valid_KeyCode(inKeyCode:inTerm) = *zero
     c**                 callp     Qp0zLprintf(inmsg + X'25')
     c**                 clear                   @@RunType
     c**                 endif
      *** End   Delete *** 12/23/19 ***************************************

     c                   if        @@RunType <> ' '
     C                   read      wxgsl
     C                   dow       not %eof(wxgsl)
     C                   if        gsiefl = 'I'

     c                   if        @@RunType = 'C' or @@RunType = 'B'
     c                   eval      ##ClmFlg = 'Y'
     c                   else
     c                   eval      ##ClmFlg = ' '
     c                   endif                                                  @@Runtype = C or B

     c                   movel     gsco#         ##ClmCo#
     c                   movel     gsfnd         ##ClmFnd
     c                   eval      ##ClmFyr = *zeros
     c                   eval      ##ClmCase= *zeros
      *
     c                   if        @@RunType = 'P' or @@RunType = 'B'
     c                   eval      ##EmpFlg = 'Y'
     c                   else
     c                   eval      ##EmpFlg = ' '
     c                   endif                                                  @@Runtype = P or B

     c                   movel     gsco#         ##EmpCo#
     c                   movel     gsfnd         ##EmpFnd
     c                   eval      ##EmpEmp# = *zeros
     c                   eval      ##EmpDiv  = *zeros

     c                   call      'SC811R'
     c                   parm                    ##ClmFlg          1
     c                   parm                    ##ClmCo#          3
     c                   parm                    ##ClmFnd          3
     c                   parm                    ##ClmFyr          3
     c                   parm                    ##ClmCase         7
     c                   parm                    ##EmpFlg          1
     c                   parm                    ##EmpCo#          3
     c                   parm                    ##EmpFnd          3
     c                   parm                    ##EmpEmp#         9
     c                   parm                    ##EmpDiv          5

     c                   endif                                                  gsiefl = 'I'

     C                   read      wxgsl

     c                   enddo                                                  not %eof(WXGSL)
     c                   endif                                                  @@RunType <> ' '


     C                   eval      *inlr = *on
     C                   return
