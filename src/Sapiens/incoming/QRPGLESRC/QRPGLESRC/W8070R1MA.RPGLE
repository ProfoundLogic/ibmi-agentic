     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     h/title w8070r1ma - build workfile for massachusetts noi
     H*****************************************************************
     H* Program Description:  This pgm builds the workfile for the    *
     H* Massachusetts NOI Forms                                       *
     H*                                                               *
     H* Called from:  W8070CMA                                        *
     H* Calls to:  None                                               *
     H*                                                               *
     H*****************************************************************
     H* Created: 05/18/06  C: 132   Project: 129WIz                   *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*****************************************************************
     fwmcmlr    if   e           k disk
     fwmeml     if   e           k disk
     fwmcdl     if   e           k disk
     fwxnil     o    e           k disk
     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0
     d psds          esds                  extname(szq1p)
      *
     d ##co#           s              3
     d qqfrst          s             15
     d qqlast          s             56
     d qqterm          s              1
      *
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     c     cmky          klist
     c                   kfld                    kkco#
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     c     cdky          klist
     c                   kfld                    cmss#
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     c     emky          klist
     c                   kfld                    kkco#
     c                   kfld                    cmfnd
     c                   kfld                    cmemp#
     c                   kfld                    cmdiv
     C*
     c     *entry        plist
     c                   parm                    ##co#
     C*
     c     *like         define    cmco#         kkco#
     C*
     c                   move      ##co#         kkco#
     c                   time                    dsdttm
     C*
     C*  START READING THE CLAIMS FILE
     C*
     c     cmky          chain     pcm01                              60
     c                   dow       (not *in60)
     C*
     c                   clear                   pni01
     C*
     C*  GET THE CLAIMANT NAME
     C*
     c     cdky          chain     pcd01                              60
     c                   if        (not *in60)
     C*
     c                   movel     cdmid         qqlast
     c                   movel     cdfsn         qqfrst
     c                   move      '1'           qqterm
     c                   call      'SRFRLS'
     c                   parm                    qqlast
     c                   parm                    qqfrst
     c                   parm                    qqterm
     C*
     c                   movel     qqlast        qqfrst
     c                   movel     cdlsn         qqlast
     c                   movel     ' '           qqterm
     c                   call      'SRNAME'
     c                   parm                    qqlast
     c                   parm                    qqfrst
     c                   parm                    qqterm
     C*
     c                   movel     qqlast        nicnam
     c                   endif
     C*
     C*  LOAD THE EMPLOYERS NAME
     C*
     c     emky          chain     pem01                              60
     c  n60              movel     emname        nienam
     C*
     C*  WRITE THE WORKFILE RECORD
     C*
     c                   eval      nico# = kkco#
     c                   eval      nifnd = cmfnd
     c                   eval      nifyr = cmfyr
     c                   eval      nicase = cmcase
     c                   eval      niemp# = cmemp#
     c                   eval      nidiv = cmdiv
     c                   eval      niidt = cmidt
     c                   eval      nientd = cmentd
     c                   eval      nidt = (dsdate * 10000.01)
     c                   eval      dstime = (dstime + 1)
     c                   eval      nitime = dstime
     c                   move      q1user        niuser
     c                   move      q1pgm         nipgm
     c                   write     pni01
     C*
     C*  GET THE NEXT CLAIM
     C*
     c     cmky          reade     pcm01                                  60
     c                   enddo
     C*
     C*  END OF PROGRAM
     C*
     c                   move      '1'           *inlr
