      * (C) Copyright Fiserv Insurance Solutions, Inc., 2007                                       *
      *============================================================================================*
      /title CPYEMP# - Copies Employer Policy Information for CEU                                  *
     H option(*nodebugio: *srcstmt)                                                                *
     H debug(*yes)                                                                                 *
      *============================================================================================*
      *
      *  Pass this program the policy information and it writes new records for the CEU.  It will
      *  renumber the policy info and change the name if the new fields are populated.  If the
      *  delete flag is passed as a 'Y', it will delete the old record. The following is an
      *  example of how to call this program from the command line. The fifth parm is the delete
      *  flag.  The New Name parm is 40 char.  All of the other parms are of correct size and are
      *  all char.
      *
      *  CALL CPYCEUEMP ('CO#' 'FND' 'EMPLOYER ' 'DIVIS' ' ' 'NEW NAME                ' 'CO#' 'FND'
      *                  'EMPLOYER ' 'DIVIS')
      *
      *
      *=============================================================================================
      * REVISIONS:                                                                                 *
      *                                                                                            *
      *=============================================================================================
     fwmeml     uf a e           k disk
     fwdell     uf a e           k disk
     fwdehl     uf a e           k disk
     fwmeal     uf a e           k disk
     fwdxhl2    uf a e           k disk
     d*entry
     d CPYEMP#         pr                  extpgm('CPYEMP#')
     d  @@co#                         3
     d  @@fnd                         3
     d  @@emp                         9
     d  @@div                         5
     d  @@delete                      1
     d  newName                      40
     d  newco#                        3
     d  newfnd                        3
     d  newemp                        9
     d  newdiv                        5
     d CPYEMP#         pi
     d  @@co#                         3
     d  @@fnd                         3
     d  @@emp                         9
     d  @@div                         5
     d  @@delete                      1
     d  newName                      40
     d  newco#                        3
     d  newfnd                        3
     d  newemp                        9
     d  newdiv                        5
      *=============================================================================================
    d* Stand Alone Fields - TOP
    d*=============================================================================================
     d   kkco#         s                   like(EMCO#)
     d   kkfnd         s                   like(EMFND)
     d   kkemp#        s                   like(EMEMP#)
     d   kkdiv         s                   like(EMDIV)
    c*=============================================================================================
      *  KlIST COLLECTION
    c*=============================================================================================
     c     oldkey        klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
    c*=============================================================================================
      *  MAINLINE PROCESSING
    c*=============================================================================================
     c                   move(p)   @@co#         kkco#
     c                   move(p)   @@fnd         kkfnd
     c                   move(p)   @@emp         kkemp#
     c                   move(p)   @@div         kkdiv

     c     oldkey        chain     pem01
     c                   if        %found
     c                   if        newco# <> *blanks
     c                   move(p)   newco#        emco#
     c                   endif
     c                   if        newfnd <> *blanks
     c                   move(p)   newfnd        emfnd
     c                   endif
     c                   if        newemp <> *blanks
     c                   move(p)   newemp        ememp#
     c                   endif
     c                   if        newdiv <> *blanks
     c                   move(p)   newdiv        emdiv
     c                   endif
     c                   if        newName <> *blanks
     c                   movel(p)  newName       emname
     c                   endif
     c                   write     pem01
     c                   if        @@delete = 'Y'
     c                   delete    pem01
     c                   endif
     c                   endif

     c     oldkey        setll     pel01
     c     oldkey        reade     pel01
     c                   dow       not(%eof)
     c                   if        newco# <> *blanks
     c                   move(p)   newco#        elco#
     c                   endif
     c                   if        newfnd <> *blanks
     c                   move(p)   newfnd        elfnd
     c                   endif
     c                   if        newemp <> *blanks
     c                   move(p)   newemp        elemp#
     c                   endif
     c                   if        newdiv <> *blanks
     c                   move(p)   newdiv        eldiv
     c                   endif
     c                   write     pel01
     c                   if        @@delete = 'Y'
     c                   delete    pel01
     c                   endif
     c     oldkey        reade     pel01
     c                   enddo
     c
     c     oldkey        setll     peh01
     c     oldkey        reade     peh01
     c                   dow       not(%eof)
     c                   if        newco# <> *blanks
     c                   move(p)   newco#        ehco#
     c                   endif
     c                   if        newfnd <> *blanks
     c                   move(p)   newfnd        ehfnd
     c                   endif
     c                   if        newemp <> *blanks
     c                   move(p)   newemp        ehemp#
     c                   endif
     c                   if        newdiv <> *blanks
     c                   move(p)   newdiv        ehdiv
     c                   endif
     c                   write     peh01
     c                   if        @@delete = 'Y'
     c                   delete    peh01
     c                   endif
     c     oldkey        reade     peh01
     c                   enddo
     c
     c     oldkey        setll     pxh01
     c     oldkey        reade     pxh01
     c                   dow       not(%eof)
     c                   if        newco# <> *blanks
     c                   move(p)   newco#        xhco#
     c                   endif
     c                   if        newfnd <> *blanks
     c                   move(p)   newfnd        xhfnd
     c                   endif
     c                   if        newemp <> *blanks
     c                   move(p)   newemp        xhemp#
     c                   endif
     c                   if        newdiv <> *blanks
     c                   move(p)   newdiv        xhdiv
     c                   endif
     c                   write     pxh01
     c                   if        @@delete = 'Y'
     c                   delete    pxh01
     c                   endif
     c     oldkey        reade     pxh01
     c                   enddo
     c
     c     oldkey        setll     pea01
     c     oldkey        reade     pea01
     c                   dow       not(%eof)
     c                   if        newco# <> *blanks
     c                   move(p)   newco#        eaco#
     c                   endif
     c                   if        newfnd <> *blanks
     c                   move(p)   newfnd        eafnd
     c                   endif
     c                   if        newemp <> *blanks
     c                   move(p)   newemp        eaemp#
     c                   endif
     c                   if        newdiv <> *blanks
     c                   move(p)   newdiv        eadiv
     c                   endif
     c                   write     pea01
     c                   if        @@delete = 'Y'
     c                   delete    pea01
     c                   endif
     c     oldkey        reade     pea01
     c                   enddo
     c
     c                   seton                                        lr
