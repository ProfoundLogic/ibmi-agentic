     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')

    H/TITLE SR04305  - CA - Voluntary Comp and Emp Liability Cov End
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
     * WC040305                                                      *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 10/06/04                                          *
     * Programmer: C:61                                              *
     * Project   : 132WAC                                            *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      *                For single print endorsements provide specific *
      *                group and employer so that correct sig is found*
     *                                                               *
     * 06/05/25 C:291 Support: 119151                                *
      *                Turn writeInCopyBook = *Off when WriteMultiRcd *
      *                is used so it doesn't request another XMLPREP  *
      *                record and 2 iDarts entries are not created    *
      *                when single form is requested.                 *
     *                                                               *
     * 12/27/24 C:300 Project: WC2994                                *
      *               -Redesign of Addition Entry pgm for WC040305    *
     *                                                               *
     * 10/27/22 C:333 Project: WC2834 (Recompile)                    *
      *               -Endorsement Recognition of Cancellation        *
     *                                                               *
     * 09/15/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
     *                                                               *
     * 07/15/16 C:136 Project: WC2449                                *
      *                Add electronic reporting
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
    
     ** Copybook F specs
     F/COPY SCOPYEN01F
    
     ** Additional Information File
     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)
      *** Beg Del *** 12/27/24 *************************************************
     F*****WXPEL     UF A E           K DISK
     F*****WXPEL1    IF   E           K DISK    RENAME(PPE01:PPE02)
      *** End Del *** 12/27/24 *************************************************
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      *
     ** Copybook D specs
     D/COPY SCOPYEN01D
     D/COPY WWCPOLDAT4
    
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqForm          s             15a   inz('WC040305  ')
    
     ** Additional entry program
      *** Beg Chg *** 12/27/24 *************************************************
     D***** addpgm          c                   const('SR04305I  ')
     D addpgm          c                   const('SR04305I2 ')
      *** End Chg *** 12/27/24 *************************************************
    
    
     *=========================================================================
     *  Standalone Fields
     *=========================================================================
     d xxdt            s              7  0
      *** Beg Del *** 12/27/24 *************************************************
     d***** xxunid          s                   like(PEUNID)
     d***** x1end#          s                   like(PEEND#)
      *** End Del *** 12/27/24 *************************************************
    
     *entry parameters
     D SR04305         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR04305         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
    
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
    
     ** Additional Input
    
     C     fmky3         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxForm
    
      *** Beg Del *** 12/27/24 *************************************************
     C*****PEKEY         KLIST
     C*****              KFLD                    XXCO#
     C*****              KFLD                    XXFND
     C*****              KFLD                    XXEMP#
     C*****              KFLD                    XXFYR
     C*****              KFLD                    XXDT              7 0

     C*****PEKEY1        KLIST
     C*****              KFLD                    XXCO#
     C*****              KFLD                    XXFND
     C*****              KFLD                    XXEMP#
     C*****              KFLD                    XXFYR

     C*****PEKEY2        KLIST
     C*****              KFLD                    XXCO#
     C*****              KFLD                    XXFND
     C*****              KFLD                    XXEMP#
     C*****              KFLD                    XXFYR
     C*****              KFLD                    XXUNID
     C*****              KFLD                    X1END#
      *** Beg Del *** 12/27/24 *************************************************
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs
     C/COPY SCOPYEN01C
    
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
    
     c                   clear                   E04305
     c                   move      *blank        xxfrst            1
     C                   open      wxfmp1
    
      *** Beg Chg *** 12/27/24 *************************************************
     C*****fmky3         chain     pfm02                              60
    
     ** Move the custom fields to print
     C*****              movel     f3c401        fmc401
     C*****              movel     f3c402        fmc402
     C*****              movel     f3c403        fmc403
     C*****              movel     f3c404        fmc404
     C*****              movel     f3c405        fmc405
     C*****              movel     f3c406        fmc406
     C*****              movel     f3c407        fmc407
     C*****              movel     f3c408        fmc408
     C*****              movel     f3c409        fmc409
     C*****              movel     f3c410        fmc410
     C*****              movel     f3c411        fmc411
     C*****              movel     f3c412        fmc412
     C*****              movel     f3c413        fmc413
     C*****              movel     f3c414        fmc414
     C*****              movel     f3c415        fmc415
     C*****              movel     f3c416        fmc416
     C*****              movel     f3c417        fmc417
     C*****              movel     f3c418        fmc418
     C*****              movel     f3c419        fmc419
     C*****              movel     f3c420        fmc420
     C*****              movel     f3c421        fmc421
     C*****              movel     f3c422        fmc422
     C*****              movel     f3c423        fmc423
     C*****              movel     f3c424        fmc424
     C*****              movel     f3c425        fmc425
     C*****              movel     f3c426        fmc426
     C*****              movel     f3c427        fmc427
     C*****              movel     f3c428        fmc428
     C*****              movel     f3c429        fmc429
     C*****              movel     f3c430        fmc430
      /Free
       Exsr OpenWorkFile;

       Setll fmky3 pfm02;
       Reade fmky3 pfm02;
       Dow not %eof(wxfmp1);
          FMc601 = %subst(F3500a1:1:60);
          Exsr WriteMultiRcd;

          Reade fmky3 pfm02;
       Enddo;

       Exsr CloseWorkFile;
      /End-Free
      *** End Chg *** 12/27/24 *************************************************
    
     C                   close     wxfmp1
    
     c                   call      'SRCHKPGM'
     c                   parm      'W1054R   '   #1pgm            10
     c                   parm      *blank        #1flag            1

     c                   if        #1flag = '1'
     c                   leavesr
     c                   endif

      *** Beg Del *** 12/27/24 *************************************************
      * Accumulation of data into WXPEP not needed
      *
      * Get latest UNID
      *
     c*****              move      *hival        xxdt
     c*****pekey         setgt     ppe02
     c*****pekey1        readpe    ppe02                                  60
     c******in60         ifeq      *off
     c*****              z-add     PEUNID        xxunid
     c*****              endif
      *
      * Copy data into the record.
      *
     c*****              movel     xxform        x1end#
     c*****pekey2        chain     ppe01
     c*****              if        %found

     c*****FMC401        ifne      *blanks
     c*****              movel(p)  FMC401        x5nm1
     c*****              movel(p)  FMC402        x5nm2
     c*****              movel(p)  FMC403        x5nm3
     c*****              movel(p)  e04305        PEDATA
     c*****              update    ppe01
     c*****              endif                                                  FMC401 IFNE

     c*****FMC404        ifne      *blanks
     c*****              movel(p)  FMC404        x5nm1
     c*****              movel(p)  FMC405        x5nm2
     c*****              movel(p)  FMC406        x5nm3
     c*****              movel(p)  e04305        PEDATA
     c*****              write     ppe01
     c*****              endif                                                  FMC404 IFNE

     c*****FMC407        ifne      *blanks
     c*****              movel(p)  FMC407        x5nm1
     c*****              movel(p)  FMC408        x5nm2
     c*****              movel(p)  FMC409        x5nm3
     c*****              movel(p)  e04305        PEDATA
     c*****              write     ppe01
     c*****              endif                                                  FMC407 IFNE

     c*****FMC410        ifne      *blanks
     c*****              movel(p)  FMC410        x5nm1
     c*****              movel(p)  FMC411        x5nm2
     c*****              movel(p)  FMC412        x5nm3
     c*****              movel(p)  e04305        PEDATA
     c*****              write     ppe01
     c*****              endif                                                  FMC410 IFNE

     c*****FMC413        ifne      *blanks
     c*****              movel(p)  FMC413        x5nm1
     c*****              movel(p)  FMC414        x5nm2
     c*****              movel(p)  FMC415        x5nm3
     c*****              movel(p)  e04305        PEDATA
     c*****              write     ppe01
     c*****              endif                                                  FMC413 IFNE

     c*****FMC416        ifne      *blanks
     c*****              movel(p)  FMC416        x5nm1
     c*****              movel(p)  FMC417        x5nm2
     c*****              movel(p)  FMC418        x5nm3
     c*****              movel(p)  e04305        PEDATA
     c*****              write     ppe01
     c*****              endif                                                  FMC416 IFNE

     c*****FMC419        ifne      *blanks
     c*****              movel(p)  FMC419        x5nm1
     c*****              movel(p)  FMC420        x5nm2
     c*****              movel(p)  FMC421        x5nm3
     c*****              movel(p)  e04305        PEDATA
     c*****              write     ppe01
     c*****              endif                                                  FMC419 IFNE

     c*****FMC422        ifne      *blanks
     c*****              movel(p)  FMC422        x5nm1
     c*****              movel(p)  FMC423        x5nm2
     c*****              movel(p)  FMC424        x5nm3
     c*****              movel(p)  e04305        PEDATA
     c*****              write     ppe01
     c*****              endif                                                  FMC422 IFNE

     c*****FMC425        ifne      *blanks
     c*****              movel(p)  FMC425        x5nm1
     c*****              movel(p)  FMC426        x5nm2
     c*****              movel(p)  FMC427        x5nm3
     c*****              movel(p)  e04305        PEDATA
     c*****              write     ppe01
     c*****              endif                                                  FMC425 IFNE

     c*****FMC428        ifne      *blanks
     c*****              movel(p)  FMC428        x5nm1
     c*****              movel(p)  FMC429        x5nm2
     c*****              movel(p)  FMC430        x5nm3
     c*****              movel(p)  e04305        PEDATA
     c*****              write     ppe01
     c*****              endif                                                  FMC428 IFNE

     c*****              endif                                                  %found
      *** End Del *** 12/27/24 *************************************************

     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
      //*** Beg Add ***** 06/05/25 **************************************
       writeInCopyBook = *Off;
      //*** End Add ***** 06/05/25 **************************************
     C                   endsr
    
    
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
    
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
     C                   move      *on           crtfrm
     C                   endsr
    
    
