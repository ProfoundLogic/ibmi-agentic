     H*  (C) Copyright Fiserv, Inc., 2007
    H*
     H/TITLE WA068R - API to retrieve provider bill # using EOB#
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:  API to retrieve provider bill # using   *
     H*                       EOB#                                    *
     H*          Return code @@RTCD (1A)                              *
     H*                ' ' - Normal return without error              *
     H*                'X' - Error                                    *
     H* CALLED BY:                                                    *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* CREATED: 07/24/07  C:191 PROJECT 132WIZ                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*****************************************************************
     Ffaeal1    if   e           k disk
     D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@co#           s              3
     D @@fnd           s              3
     D @@fyr           s              3
     D @@case          s              7
     D @@eob#          s              9
     D @@sinv          s             35
     D @@rtcd          s              1
     D kkco#           s              3  0
     D kksub#          s              5  0
     D kkfnd           s              3  0
     D kkfyr           s              3  0
     D kkcase          s              7  0
     D kkeob#          s              9  0
     D ##rtcd          s              1
     D ##co#           s              3
     D ##wcf#          s              3
     D ##sub#          s              5
     D ##fnd           s              3
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     C*
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@fyr
     C                   parm                    @@case
     C                   parm                    @@eob#
     C                   parm                    @@sinv
     C                   parm                    @@rtcd
     C*
     C*** partial key list to check claim record
     C     eaky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C*** Set key field values
     C                   move      @@co#         kkco#
     C                   move      @@fyr         kkfyr
     C                   move      @@case        kkcase
     C                   move      @@eob#        kkeob#
     C*
     C*** Set return values to default
     C                   eval      @@rtcd = 'X'
     C                   eval      @@sinv = *blanks
     C*** Call API to convert WC group to MC Group/Sub
     C                   call      'FA700R'
     C                   parm      *blanks       ##rtcd
     C                   parm      @@co#         ##co#
     C                   parm      @@fnd         ##wcf#
     C                   parm      *blanks       ##sub#
     C                   parm      *blanks       ##fnd
     C*****
     C                   if        ##rtcd = *blanks
     C                   move      ##sub#        kksub#
     C                   move      ##fnd         kkfnd
     C     eaky          chain     pea01
     C                   if        %found
     C                   eval      @@sinv = easinv
     C                   eval      @@rtcd = ' '
     C                   endif                                                  %found
     C                   endif                                                  ##rtcd = *blanks
     C*
     C                   eval      *inlr = *on
