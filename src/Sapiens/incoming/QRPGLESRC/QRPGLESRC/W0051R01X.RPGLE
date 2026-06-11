     H* (C) Copyright Fiserv, Inc., 2007
     H*********************************************************************
     H/TITLE W0051R01X - Deletion of Employer Detail Related Files - Clt
     H*********************************************************************
     H*  DESCRIPTION:  This is a shell program.  The base version of this *
     H*                 code should only accept a call and return to the  *
     H*                 calling program.  When a client specific file is  *
     H*                 in need of deletion, the base copy should be      *
     H*                 copied to the client environment and the specific *
     H*                 files added to that version.                      *
     H*                                                                   *
     H*  Called from:                                                     *
     H*    W0051R01 - Deletion of Employer Detail Related Files           *
     H*                                                                   *
     H*  Calls to:                                                        *
     H*                                                                   *
     H*  CreateD: 02/14/07      C:33      Seq#: WA045/54881               *
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H*********************************************************************
      *
     C*
     C     deleteKey     klist
     C                   kfld                    @@Co#
     C                   kfld                    @@Fnd
     C                   kfld                    @@Emp#
     C                   kfld                    @@Div
     C                   kfld                    @@Fyr
     C*
     C     deleteKey2    klist
     C                   kfld                    @@Co#
     C                   kfld                    @@Fnd
     C                   kfld                    @@Fyr
     C                   kfld                    @@Emp#
     C                   kfld                    @@Div
     C*
     C     *like         define    @@Qte         xxQte
     C*
     C     *entry        plist
     C                   parm                    @@Co#             3 0
     C                   parm                    @@Fnd             3 0
     C                   parm                    @@Emp#            9 0
     C                   parm                    @@Div             5 0
     C                   parm                    @@Fyr             3 0
     C                   parm                    @@QTE             1
     C*
     C                   clear                   xxQte
     C*
     C                   if        %parms >= 6
     C                   eval      xxQte = @@Qte
     C                   endif                                                  Q1PRMS IFGE 6
     C*
     C* Use this subroutine to delete from files that are only in the
     C*  LIVE environment
     C*
     C                   if        xxQte = 'Q'
     C                   exsr      DltLiveOnly
     C                   endif
     C*
     C*
     C                   eval      *inlr = *on
     C*
     C*====================================================================
     C* LIVE Only deletions
     C*====================================================================
     C* Use this subroutine to delete from files that are only in the
     C*  LIVE environment
     C     DltLiveOnly   begsr
     C*
     C*
     C                   endsr
