     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR00320A - ENDORSEMENT ATTACH/PRINT PROGRAM
     *===============================================================*
     * WC000320A - Labor Contractor Endorsement                      *
     *                                                               *
     * PROGRAM DESCRIPTION:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * CREATED   : 05/30/01                                          *
     * PROGRAMMER: C: 90                                             *
     * PROJECT   : CM027                                             *
     *                                                               *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     *  08/21/25 C:333 Support: 148/117703  (Recompile)              *
      *                For single print endorsements provide specific *
      *                group and employer so that correct sig is found*
     *                                                               *
     * 10/20/22 C:333 Project : WC2834 (Recompile)                   *
      *                        Endorsement Recognition of Cancellation*
     *                                                               *
     * 03/02/21 C:180 Support 93817- Clt 148                         *
      *                        Do not use default parms.              *
     *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
     * ------------------------------------------------------------- *
     *===============================================================*
     * NOTES:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
     *                                                               *
     *  qqmode = 1 - Called to determine if form should be attached. *
     *  qqmode = 2 - Called to print endorsement.                    *
     *  qqmode = 3 - Called for additional input.                    *
     *  qqmode = 4 - Called for midterm processing.                  *
     *===============================================================*

     Fwdl2l2    if   e           k disk

      /COPY SCOPYEN01F

     D                 ds
     D state                          2
     D  fmfl01                        1    overlay(state)
     D  fmfl02                        1    overlay(state:2)

     D xxefdt          s                   like(fmeedt)

     ** Copybook D specs
     D/COPY SCOPYEN01D
     *=========================================================================
     *  Copybook required constants
     *=========================================================================

     ** Form ID
     D qqform          c                   const('WC000320A ')

     ** Additional entry program
     D addpgm          c                   const('SR00320I  ')
     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     ** Entry parameters
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqemp#            9
     C                   parm                    qqdiv             5
     C                   parm                    qqfyr             3
     C                   parm                    qqmode            1
     C                   parm                    qqrrn             9 0
     C                   parm                    qqmod2            1
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     ** Alternate Employer file
     C     l2Key         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs (PLISTs, KLISTs and mainline)
     C/COPY SCOPYEN01C
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information
     *=========================================================================
     C     othinf        begsr
    
     ** Write the output record
     C     l2Key         setll     pl201
     C     l2Key         reade     pl201                                  60
                         If *in60 = *Off;
                            If not %Open(WXFMPMULTI);
                               Exsr OpenWorkFile;
                            Endif;
                         Endif;
    
     C                   dow       *in60 = *off
    
     C                   if        l2efdt = *zeros
     C                   eval      srdti = elpfdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxefdt

     C                   else
     c                   eval      srdti = l2efdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxefdt
     C                   endif

     C                   if        xxefdt = fmeedt
     C
     C                   movel     l2nam1        fmnm01
     C                   movel     l2adr1        fmc401
    
     C                   if        l2adr2 <> *blank
     C                   movel     l2adr2        fmc402
     C                   endif
    
     C                   move      l2zip         xxzip
     C     l2cty         cat       ',':0         fmc403
     C                   cat       l2st:1        fmc403
     C                   cat       xxzip1:1      fmc403
    
     C                   if        xxzip2 <> *blank
     C                             and xxzip2 <> *zero
     C                   cat       '-':0         fmc403
     C                   cat       xxzip2:0      fmc403
     C                   endif
    
     C*****************  movel     l2emst        xxst
     C                   movel     l2emst        fmc101
     C                   movel     l2proj        fmc404
    
     C                   exsr      WriteMultiRcd
     C                   clear                   fmnm01
     C                   clear                   fmc401
     C                   clear                   fmc402
     C                   clear                   fmc403
     C                   clear                   fmc404
     C                   clear                   fmc101
     C                   clear                   fmfl01
     C                   clear                   fmfl02
     C                   clear                   xxzip

     C                   endif
    
     C     l2Key         reade     pl201                                  60
     C                   enddo
    
                         If %Open(WXFMPMULTI);
                            Exsr CloseWorkFile;
                         Endif;
     C                   endsr
      /Free
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
       Begsr Pgmstr;
        UseDfltAddParms = *off;                  //Added 03/02/21
        WriteInCopybook = *Off;
       Endsr;
      /End-Free
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
    
      ** This is a manual attach endorsement with additional input
     C                   move      *on           crtfrm
    
     C                   endsr
