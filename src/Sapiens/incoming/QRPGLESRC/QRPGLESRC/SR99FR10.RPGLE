      * (C) Copyright FISERV Insurance Solutions, Inc., 2006
      *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H/TITLE SR99FR10 - CA Approved Form 10
      *===============================================================*
      * PROGRAM DESCRIPTION:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      *===============================================================*
      * REVISIONS:                                                    *
      *                                                               *
      * 04/12/06 C:61  PROJECT 132WFF                                 *
      *                Created.                                       *
      *                                                               *
      *===============================================================*
      * NOTES:                                                        *
      *                                                               *
      *  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
      *                                                               *
      *  qqmode = 1 - Called to determine if form should be attached. *
      *  qqmode = 2 - Called to print endorsement.                    *
      *  qqmode = 3 - Called for additional input.                    *
      *  qqmode = 4 - Called for midterm processing.                  *
      *===============================================================*

     Fwmfrl     if   e           k disk


      ** Copybook F specs
     F/COPY SCOPYEN01F


      *=========================================================================
      *  Definition Specs
      *=========================================================================
     D/COPY WWCPOLDAT4

     D INDS            ds
     D  FMC401
     D  FMC402
     D  FMC403
     D  FMC404
     D  FMC405
     D  FMC406
     D  FMC407
     D  FMC408
     D  FMC409
     D  FMC410
     D  FMC411
     D  FMC412
     D  FMC413
     D  FMC414
     D  FMC415
     D  FMC416
     D  FMC417
     D  FMC418
     D  FMC419
     D  FMC420
     D  FMC421
     D  FMC422
     D  FMC423
     D  FMC424
    D IN                            80a   OVERLAY(INDS)
     D                                     DIM(12)

     D issDS           ds
     D  FMC425
     D  FMC202
    D ISS                           60a   OVERLAY(IssDS)

     D xxefdt          s                   like(fmeedt)


      ** Copybook D specs
     D/COPY SCOPYEN01D
      *=========================================================================
      *  Copybook required constants
      *=========================================================================

      ** Form ID
     D qqform          c                   const('WC99FR10  ')

      ** Additional entry program
     D addpgm          c                   const('SR99FR10I ')


      *=========================================================================
      *  PLIST definitions
      *=========================================================================

      ** Entry parameters
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqemp#            9
     C                   parm                    qqdiv             5
     C                   parm                    qqfyr             3
     C                   parm                    qqmode            1
     C                   parm                    qqrrn             9 0
     C                   parm                    qqmod2            1


      *=========================================================================
      *  KLIST definitions
      *=========================================================================

      *  Alternate Employer file

     C     frky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxfyr


      *=========================================================================
      *  Main
      *=========================================================================
    
      ** Copybook C specs (PLISTs, KLISTs and mainline)
     C/COPY SCOPYEN01C


      *=========================================================================
      *  OTHINF - Subroutine to load form specific information
      *=========================================================================
     C     othinf        begsr
    
     C     frky          setll     pfr01
     C     frky          reade     pfr01                                  60
    
     C                   dow       not %eof

     c                   movel(p)  frin01        in(1)
     c                   movel(p)  frin02        in(2)
     c                   movel(p)  frin03        in(3)
     c                   movel(p)  frin04        in(4)
     c                   movel(p)  frin05        in(5)
     c                   movel(p)  frin06        in(6)
     c                   movel(p)  frin07        in(7)
     c                   movel(p)  frin08        in(8)
     c                   movel(p)  frin09        in(9)
     c                   movel(p)  frin10        in(10)
     c                   movel(p)  frin11        in(11)
     c                   movel(p)  frin12        in(12)

     c                   movel(p)  frpol#        fmc201
     c                   movel(p)  frissu        iss
     c                   movel(p)  frto          fmc426
     c                   movel(p)  frof          fmc427
     c                   movel(p)  frdaat        fmc203
     c                   movel(p)  frthis        fmc101
     c                   movel(p)  frdaof        fmc204
     c                   z-add     fryear        fmd601
    
     C                   write     pfm01
      *
     C                   clear                   in
     c                   clear                   fmc201
     c                   clear                   iss
     c                   clear                   fmc426
     c                   clear                   fmc427
     c                   clear                   fmc203
     c                   clear                   fmc101
     c                   clear                   fmc204
     c                   clear                   fmd601
    
     C     frky          reade     pfr01                                  60
     C                   enddo
    
     C                   endsr


      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     pgmstr        begsr
     C                   endsr


      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     pgmend        begsr
     C                   endsr


      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     valid         begsr
    
      ** This is a manual attach endorsement with additional input

     C                   move      *on           crtfrm
    
     C                   endsr


