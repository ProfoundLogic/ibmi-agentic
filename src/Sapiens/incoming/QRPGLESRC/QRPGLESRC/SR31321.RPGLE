     *===============================================================*
     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR31321 - OPT LABOR CONTRACTOR EXCLUSION ENDORSEMENT
     *===============================================================*
     * WC310321 - OPT Labor Contractor Exclusion Endorsement         *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 12/08/06                                          *
     * Programmer: C: 184                                            *
     * Project   : WC1505                                            *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
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
     F/COPY SCOPYEN01F
      *===========================================================
      * Prototypes
      *===========================================================
      *Entry parameters
     D SR31321         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR31321         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
     D/COPY SCOPYEN01D
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     D qqform          s             15    inz('WC310321  ')
     D addpgm          c                   const('SRxxxxxxxx')
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     C                   endsr
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
      /Free
       Begsr Valid;
       Exsr getFnd;
       Select;
       When FNpeof = *blank;
          crtFrm = *Off;
       When FNpeof = 'M' and ELemp# <> 1;
          crtFrm = *On;
       When FNpeof = 'S' and ELdiv  <> 0;
          crtFrm = *On;
       Endsl;
       Endsr;
      /End-Free
