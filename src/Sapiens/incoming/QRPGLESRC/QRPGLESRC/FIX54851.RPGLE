    H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2006
     H/TITLE FIX54851
     H**************************************************************************
    H*  Populate X4FDOS and X4TDOS
    H*
     H**************************************************************************
    H*  Created on 09/18/2006 by C:079 for Support 54851
     H**************************************************************************
    H*  Revisions:
    H*
     H**************************************************************************
    H*  Indicators used:
    H*
     H**************************************************************************
    F*
    F* Import files
    F*
     Ffax4p     uf   e           k disk
     Ffax5l3    if   e           k disk    rename(px501:px503) prefix(d1:2)
     Ffax5l4    if   e           k disk    rename(px501:px504) prefix(d2:2)
    D*****************************************************************
    D* Program constants.
    D*****************************************************************
    D*================================================================
    D*  User defined data structures
    D*================================================================
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D kkfdat          s                   like(d1fdat)
     D kktdat          s                   like(d1fdat)
     D kkcntl          s                   like(d1cntl)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D/COPY SCOPYWW01I
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C     x5ky3         klist
     C                   kfld                    kkcntl
     C                   kfld                    kkfdat
    C*
     C     x5ky4         klist
     C                   kfld                    kkcntl
    C*
     C     x5ky5         klist
     C                   kfld                    kkcntl
     C                   kfld                    kktdat
     C*====================================================================
     C* MAINLINE
     C*====================================================================
     C*
     C                   read      px401                                  99
     C*
     C                   dow       *in99 = *off
     C*
     C                   if        x4fdos = *blanks
     C*
     C                   eval      kkfdat = *zeros
     C                   eval      kktdat = *hival
     C                   eval      kkcntl = x4cntl
     C*
     C     x5ky4         chain     px503                              99
     C*
     C                   if        *in99 = *off
     C                   eval      x4fdos = d1fdat
     C                   endif
     C*
     C     x5ky4         setgt     px504
     C     x5ky4         readpe    px504                                  99
     C*
     C                   if        *in99 = *off
     C                   eval      x4tdos = d2tdat
     C                   endif
     C*
     C                   update    px401
     C*
     C                   endif
     C*
     C                   read      px401                                  99
     C*
     C                   enddo
     C*
     C                   eval      *inlr = *on
