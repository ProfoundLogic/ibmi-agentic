     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
    H*****************************************************************
    h/TITLE W0050R17W - Wrapper program to call W0050R17W
    H*****************************************************************
    H*                                                               *
    H*    PROGRAM DESCRIPTION:                                       *
    H*      Wrapper program with all alpha parms to call W0050R17W.  *
    H*                                                               *
    H*  Programmer: C:65    Date:  10/22/12   Project:  WC2116       *
    H*****************************************************************
    C*
    C*   ENTRY PARAMETERS
    C*
     C     *entry        plist
     C                   parm                    @@mode            1
     C                   parm                    @@co#             3
     C                   parm                    @@fnd             3
     C                   parm                    @@emp#            9
     C                   parm                    @@div             5
     C                   parm                    @@err             1
    C*
     C                   move      @@co#         @1co#             3 0
     C                   move      @@fnd         @1fnd             3 0
     C                   move      @@emp#        @1emp#            9 0
     C                   move      @@div         @1div             5 0
    C*
     C                   call      'W0050R17'
     C                   parm                    @@mode
     C                   parm                    @1co#
     C                   parm                    @1fnd
     C                   parm                    @1emp#
     C                   parm                    @1div
     C                   parm                    @@err
    C*
     C                   move      *on           *inlr
