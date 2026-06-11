     h*  (C) Copyright Fiserv Inc., 2005 - 2007                                                    *
    h** ======================================================================================== **
    h/TITLE - Import - Process Prompt                                                             *
    h** ======================================================================================== **
    h*  DESCRIPTION:                                                                              *
     h*    Prompt for user to import Claims Information from the flat file CLIMPORT.               *
     h*                                                                                            *
     h*  Called from: Menu                                                                         *
     h*                                                                                            *
     h*  Calls to: W9110C (Process Import claims - Submitter)                                      *
     h*                                                                                            *
     h*  Created: 05/08/05  by C:132    Project: 139WAI                                            *
    h** ======================================================================================== **
     h* Revisions:                                                        *
     h*                                                                                            *
     h* 01/18/19 C:295 Support: CW 76340                                                           *
     h*                Defaulting the feild value to N before displaying the screen.               *
     h*                                                                                            *
    h** ======================================================================================== **
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     fW9110F    cf   e             workstn
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

     c***** beg add ***** 07/23/08 ************************************
     c     strt          tag
     c***** end add ***** 07/23/08 ************************************
     c***** beg add ***** 01/18/19 ************************************
      /Free
         scoptn = 'N';
      /End-Free
     c***** end add ***** 01/18/19 ************************************
     c                   exfmt     wdw01

B02  c                   if        *inkc = *on
     c                   leave
     c***** beg add ***** 07/23/08 ************************************
E02  c                   elseif    (scoptn <> 'Y' and scoptn <> 'N')
     c                   eval      *in31 = *on
     c                   goto      strt
     c***** end add ***** 07/23/08 ************************************
E02  c                   else
     c                   call      'W9110C'
     c                   parm                    upoutq
     c***** beg add ***** 07/23/08 ************************************
     c                   parm      scoptn        ##optn            1
     c***** end add ***** 07/23/08 ************************************
E02  c                   endif

     c                   enddo

     c                   move      *on           *inlr
