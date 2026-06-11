     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    h**********************************************************************************************
    h/TITLE - Check Request Import                                                                *
    h**********************************************************************************************
    h*  DESCRIPTION:                                                                              *
     h*  Check Request Import Prompt                                                               *
     h*                                                                                            *
     h*  Called from: Menu                                                                         *
     h*                                                                                            *
     h*  Calls to: W8320C (Upload Sentry Billings - Submitter)                                     *
     h*                                                                                            *
     h*  Created: 05/22/06  by C:136    Project: 135WGG                                            *
    h**********************************************************************************************
     h* Revisions:                                                        *
    H*                                                                   *
    H* 11/02/10  C:183 Project: SY195                                    *
    H*                 Remove hard coding for QDLS. Call RTVDOCROOT to   *
    H*                 get the root folder name.                         *
     h*                                                                                            *
    h**********************************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     fw8320f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     f                                     infds(fids01)
     fwmupl     if   e           k disk

     d psds          esds                  extname(szq1p)
     d fids01        e ds                  extname(szq2p)
     d rstpgm          s              1
     d ##co#           s              3
      *** Beg chg ***** 11/02/10 ***********************************************
     d******* begOfPath1      c                   '/QDLS'
     d******* begOfPath2      c                   '/QDLS/PDS'
     d begOfPath1      s             11    inz
     d begOfPath2      s             15    inz
      *** End chg ***** 11/02/10 ***********************************************
     d endOfPath       c                   '/DOWNLOAD.'
     d parm1           s              3    inz(*blank)
     d directory       s             80    inz(*blank)
      *** Beg add ***** 11/02/10 ***********************************************
     D root            s             10    inz
      *** End add ***** 11/02/10 ***********************************************
    
     d GetSuffix       pr                  extpgm('SA030R')
     d  parm1                         3    const
     d  folderPrefix                 80    const

     c     upky          klist
     c                   kfld                    q1user
    
     c                   movel     q1user        scuser
     c     upky          chain     pup01

      *** Beg add ***** 11/02/10 ***********************************************
     C                   call      'RTVDOCROOT'
     C                   parm                    root
     c                   eval      begOfPath1 = '/' + %trim(Root)
     c                   eval      begOfPath2 = '/' + %trim(Root) + '/PDS'
      *** End add ***** 11/02/10 ***********************************************
     c                   callp     GetSuffix(parm1:directory)
     c                   if        directory <> *blank
     c                   eval      scpath = begOfPath1
     c                   cat       directory:0   scpath
     c                   else
     c                   eval      scpath = begOfPath2
     c                   endif
     c                   cat       endOfPath:0   scpath

     c                   dou       rstpgm = *off
     c                   move      *off          rstpgm

     c                   exfmt     wdw01

B02  c                   if        *inkc = *on
     c                   leave
E02  c                   else
     c                   move      upco#         ##co#             3
     c                   call      'W8320C'
     c                   parm                    ##co#
     c                   parm                    upoutq
E02  c                   endif

     c                   enddo

     c                   move      *on           *inlr
