     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**************************************************************************************
     H* Created on 11/08/2019 by c:79 for Support: 99/82052
     H**************************************************************************************
     H* Revisions:                                                                         *
     H*                                                                                    *
     H*                                                                                    *
     H**************************************************************************************
     D* Prototypes
     D/copy fprcase
     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@Co#             3 0
     C                   PARM                    @@Fnd             3 0
     C                   PARM                    @@Fyr             3 0
     C                   PARM                    @@Case            7 0
     C                   PARM                    @@Eob#            9 0
     C                   PARM                    @@EobRrn          9
     C*
      /Free
        @@EobRrn = %EditC(GetFaeopRrn(@@Co#:@@Fnd:@@Fyr:@@Case:@@Eob#):'X');

        *inlr = *on;
        Return;
      /End-Free

