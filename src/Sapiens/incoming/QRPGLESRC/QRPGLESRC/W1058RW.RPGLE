     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
    H*****************************************************************
    h/TITLE W1058RW - Wrapper program to call W1058R
    H*****************************************************************
    H*                                                               *
    H*    PROGRAM DESCRIPTION:                                       *
    H*      Wrapper program with all alpha parms to call W1058R.     *
    H*                                                               *
    H*  Programmer: C:65    Date:  10/03/12   Project:  WC2116       *
    H*****************************************************************
    C*
    C*   ENTRY PARAMETERS
    C*
     C     *entry        plist
     C                   parm                    @@call            1
     C                   parm                    @@kc              1
     C                   parm                    @@kl              1
     C                   parm                    @@rrn             9
    C*
     C                   move      @@rrn         @1rrn             9 0
    C*
     C                   call      'W1058R'
     C                   parm                    @@call
     C                   parm                    @@kc
     C                   parm                    @@kl
     C                   parm                    @1rrn
    C*
     C                   move      *on           *inlr
