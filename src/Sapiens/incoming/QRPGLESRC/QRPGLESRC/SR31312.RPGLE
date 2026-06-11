     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *
    H/TITLE SR31312  - NY - Non-Subject Executive Officers Endorsement
     *===============================================================*
     * WC310312  - NY Non-Subject Exec Officers Endorsement
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 12/01/06                                          *
     * Programmer: C:184                                             *
     * Project   : WC1505                                            *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
      * 10/19/22 C:342 Project: WC2834 (Recompile)                    *
      *                Endorsement Recognition of Cancellation        *
     *                                                               *
      * 02/17/17  Support 41271                                       *
      *           Use parm ADDPL1 when calling addtl info pgm.        *
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
     *
     ** Copybook F specs
     F/COPY SCOPYEN01F
     *
     ** Additional Information File
     Fwxfmp1    if   e           k disk
     F                                     rename(pfm01:pfm02)
     Fwdn1l     if   e           k disk
     *
     *
     *
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      *
     ** Copybook D specs
     D/COPY SCOPYEN01D
      *
      *
      *
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
      *
     ** Form ID
     D qqForm          s             15a   inz('WC310312  ')
      *
     ** Additional entry program
     D addpgm          c                   const('SR31312I  ')
      *
     D INM             S              2  0 INZ
     D IMC             S              2  0 INZ
      ** Names to include on form
     D                 ds
     D  nm                     1    640
     D                                     dim(16)
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
     D  FMC411               401    440
     D  FMC412               441    480
     D  FMC413               481    520
     D  FMC414               521    560
     D  FMC415               561    600
     D  FMC416               601    640
      ** Title to include on form
     D                 ds
     D  mc                     1    480
     D                                     dim(16)
     D  FMC301                 1     30
     D  FMC302                31     60
     D  FMC303                61     90
     D  FMC304                91    120
     D  FMC305               121    150
     D  FMC306               151    180
     D  FMC307               181    210
     D  FMC308               211    240
     D  FMC309               241    270
     D  FMC310               271    300
     D  FMC311               301    330
     D  FMC312               331    360
     D  FMC313               361    390
     D  FMC314               391    420
     D  FMC315               421    450
     D  FMC316               451    480

      *entry parameters
     D SR31312         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR31312         PI
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
     *
     ** Additional Input file
    
     C     n1key         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
    
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C
    
     *
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
      /Free
          WriteInCopybook = *Off;
      /End-Free
     C
     C                   Z-ADD     0             X                 2 0
     C     n1Key         setll     pn101
     C     n1Key         reade     pn101                                  60
     C
     C                   dow       *in60 = *off
     C                   eval      inm= inm+ 1
     C                   movel     N1ONAM        nm(inm)
     C                   movel     N1TITL        mc(inm)
     ** Write the output record
     C                   If        inm= 16
      /Free
          //* Write the output record
          Exsr WriteWorkFile;
      /End-Free
     C                   clear                   nm
     C                   clear                   mc
     C                   eval      inm= *zero
     C
     C                   endif
     C     n1Key         reade     pn101                                  60
     C
     C                   Enddo
     ** Write the output record
     C                   If        inm < 16
      /Free
          //* Write the output record
          Exsr WriteWorkFile;
      /End-Free
     C                   clear                   nm
     C                   clear                   MC
     C                   eval      inm= *zero
     C
     C                   endif
     C
     *
     C                   endsr
     *
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
      /Free
       UseDfltAddParms = *OFF;
      /End-Free
     C                   endsr
     *
     *
     *
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
     *
     *
     *
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
     *
     C                   move      *on           crtfrm
     *
     C                   endsr
     *
