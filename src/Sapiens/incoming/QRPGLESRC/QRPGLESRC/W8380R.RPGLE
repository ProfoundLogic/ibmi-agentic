     h*  (C) Copyright Progressive Data Solutions, Inc., 2002                                      *
    h**********************************************************************************************
    h/TITLE - Upload Premium - Prompt                                                            *
    h**********************************************************************************************
    h*  DESCRIPTION:                                                                              *
     h*    Prompt for user to upload payrolls from the flat file WXUPP.                            *
     h*                                                                                            *
     h*  Called from: Menu                                                                         *
     h*                                                                                            *
     h*  Calls to: W8380C (Upload Premium - Submitter)                                            *
     h*                                                                                            *
     h*  Created: 02/18/03  by C:136    Project: WC1115                                            *
    h**********************************************************************************************
     h* Revisions:                                                        *
     h*                                                                                            *
    h**********************************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     fw8380f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     f                                     infds(fids01)
     fwmupl     if   e           k disk
     d psds          esds                  extname(szq1p)
     d fids01        e ds                  extname(szq2p)
     D rstpgm          s              1
    
     c     upky          klist
     c                   kfld                    q1user
    
     c                   movel     q1user        scuser
     c     upky          chain     pup01

     c                   dou       rstpgm = *off
     C                   move      *off          rstpgm

     c                   exfmt     wdw01

B02  c                   if        *inkc = *on
     c                   leave
E02  c                   else
     c                   call      'W8380C'
     c                   parm                    upoutq
E02  c                   endif

     c                   enddo

     c                   move      *on           *inlr
