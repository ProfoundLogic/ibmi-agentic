     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     h/TITLE LODWC2816 - Update USL&H flag for claims that qualify
     h/DEFINE   PROFOUNDUI
     h DFTNAME(LODWC2816)
     h BNDDIR('STBNDDIR')
     h DFTACTGRP(*NO) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h**********************************************************************
     h*
     h*  Description:  This Program sets CMUSLH based on loss coverage and
     h*                runs during the base release postload process
     h*
     h*  Created on 7/14/2021 by C:79 for Project WC2816
     h**********************************************************************
     h* Revisions:                                                         *
     fwmcmp     uf   e           k Disk
     fwtgil     if   e           k Disk
     fwtgtl     if   e           k Disk
     d*=====================================================================
     d* Standalone fields top
     d*=====================================================================
     d strLsCv         s              2a
     d intCo#          s              3s 0
     d kkTbId          s                   Like(GTtbId)
     d*=====================================================================
     d* Copybooks
     d*=====================================================================
     d*=====================================================================
     d* Standalone fields bottom
     d*=====================================================================
     ddsWMCMP        e ds                  extname(WMCMP) qualified inz
     d psds          esds                  Extname(SZQ1P)

       //===================================================================
       // Main program
       //===================================================================
       Read pcm01;
       Dow Not %Eof(wmcmp);
          Exsr srUsLh;
          Read pcm01;
       Enddo;

       *inlr = *on;
       Return;
       //===================================================================
       // Subroutine srUsLh - Set CMusLh if Claim Qualifies via Loss Cov.
       //===================================================================
       Begsr srUsLh;

       // Determine if Claim qualifies for USL&H and update CMusLH
       intCo# = CMco#;
       strLsCv = CMlsCv;
       kkTbId = CMjrSt + 'LC';
       Chain (intCo#:kkTbId) pgi01;
       If Not %Found(WTGIL);
          kkTbId='  LC';
       EndIf;

       Chain (intCo#:kkTbId:strLsCv) pgt01;
       If %Found(wtgtl) and GTstat <> 'I' and %Subst(GTcf01:1:2) = '02';
          CMusLh = 'Y';
          Update pcm01;
       Else;
          CMusLh = 'N';
          Update pcm01;
       Endif;

       Endsr;
