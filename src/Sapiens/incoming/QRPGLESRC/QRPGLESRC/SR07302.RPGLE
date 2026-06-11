     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
    H/TITLE SR07302 - DE - Exclusion of Executive Officers Endorsement
     *===============================================================*
     * WC070302 - Delaware Exclusion of Executive Officers Endorse   *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 05/09/01                                          *
     * Programmer: C:90                                              *
     * Project   : CM027                                             *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 09/29/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
     *                                                               *
     * 08/24/01  C:90 PROJECT: CM027 WA107/28711                     *
     *                Read thru all divisions within the OTHINF      *
     *                subroutine.                                    *
     *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: Please add comments to the top of revisions.           *
     * ------------------------------------------------------------- *
     *===============================================================*
     * Notes:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     *                                                               *
     *  QQMODE = 1 - Called to determine if form should be attached. *
     *  QQMODE = 2 - Called to print endorsement.                    *
     *  QQMODE = 3 - Called for additional input.                    *
     *  QQMODE = 4 - Called for mid-term processing.                 *
     *===============================================================*
     ** Copybook F specs
     F/COPY SCOPYEN01F
     Fwmool     if   e           k disk
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     D ix              s              2  0
     D x               s              2  0
     D xxnm            s             40

     D                 ds
     D  nm                     1    400
     D                                     dim(10)
     D  FMC401                 1     40
     D  FMC402                41     80
     D  FMC403                81    120
     D  FMC404               121    160
     D  FMC405               161    200
     D  FMC406               201    240
     D  FMC407               241    280
     D  FMC408               281    320
     D  FMC409               321    360
     D  FMC410               361    400
      *
     ** Copybook D specs
     D/COPY SCOPYEN01D
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     ** Form ID
     D qqForm          s             15a   inz('WC070302  ')
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')

      *entry parameters
     D SR07302         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR07302         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
    C*
     C     ooky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
    C*
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C
     *
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
      /Free
       WriteInCopybook = *Off;
      /End-Free
     *
     C                   eval      ix = *zero
     C                   clear                   nm
     C     elky2         chain     pel02                              60
    
     C                   dow       *in60 = *off
     *
     C     rsky          chain     prs01                              60
     *
     C                   if        *in60 = *off
     C                             and rsjrst = enst
      *
     C     ooky          setll     poo01
     C     ooky          reade     poo01                                  60
      *
     C                   dow       *in60 = *off
      *
     C                   if        OOEXFL = 'E'
     C                             and OOSTAT <> 'I'

     C     OOOFSN        cat(p)    OOOLSN:1      xxnm
     C                   eval      x = 1
     C     xxnm          lookup    nm(x)                                  65
     C                   if        *in65 = *off
     C                   eval      ix = ix + 1
     C                   movel     xxnm          nm(ix)

     C                   endif                                                  *in65

     C                   endif

     C     ooky          reade     poo01                                  60
     C                   enddo
     C                   endif                                                  RSJRST = ENST
     C     elky2         reade     pel02                                  60
     C                   enddo
    
     C                   if        ix <> *zero
      /Free
       //* Write the output record
       Exsr WriteWorkFile;
      /End-Free
     C                   endif
     *
     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     *
     *
     C                   endsr
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     *
     *
     C                   endsr
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
     *
     ** Found a division with a matching jurisdiction state
     *
     C     ooky          setll     poo01
     C     ooky          reade     poo01                                  60
      *
     C                   dow       *in60 = *off
      *
     C                   if        OOEXFL = 'E'
     C                             and OOSTAT <> 'I'
     *
     C                   move      *on           crtfrm
     C                   leave
     *
     C                   endif
     *
     C     ooky          reade     poo01                                  60
     C                   enddo
     *
     C                   endsr
