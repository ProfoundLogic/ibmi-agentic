     hCopyright('(c) 2023 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR00407 - ENDORSEMENT FORM PROCESSING PROGRAM

      //===============================================================*
      // WC000407 - Rate Change Endorsement                            *
      //                                                               *
      // PROGRAM DESCRIPTION:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      //===============================================================*
      // Revised:
      //                                                               *
      // 01/27/23 C:291 Project: WC2863                                *
      //                State Include/Exclude Endorsement Print Control*
      //                                                               *
      // 10/26/22 C:333 Project: WC2834                                *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      // ------------------------------------------------------------- *
      //  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
      // ------------------------------------------------------------- *
      //===============================================================*
      // NOTES:                                                        *
      //                                                               *
      //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
      //                                                               *
      //  QQMODE = 1 - Called to determine if form should be attached. *
      //  QQMODE = 2 - Called to print endorsement.                    *
      //  QQMODE = 3 - Called for additional input.                    *
      //  QQMODE = 4 - Called for mid-term processing.                 *
      //===============================================================*
      /COPY SCOPYEN01F

      ** Endorsement Form Statistical Info File
     Fwmnxl     if   e           k disk

      /COPY SCOPYEN01D

     D                 DS
     D  ST                                 like(fmemst)
     D                                     DIM(52) ASCEND

     D                 DS
     D  REC                                like(fm2001)
     D                                     DIM(52) ASCEND

     D output          ds
     D  fm2001
     D  fm2002
     D  fm2003
     D  fm2004
     D  fm2005
     D  fm2006
     D  fm2007
     D  fm2008
     D  fm2009
     D  fm2010
     D  fm2011
     D  fm2012
     D  fm2013
     D  fm2014
     D  fm2015
     D  fm2016
     D out                                 OVERLAY(output)
     D                                     LIKE(fm2001) DIM(16)

     D efdt            ds
     D  emefd1
     D  emefd2
     D  emefd3
     D  emefd4
     D  emefd5
     D ef                                  OVERLAY(efdt)
     D                                     LIKE(emefd1) DIM(5)
     D                                     inz

     D cxdt            ds
     D  emcad1
     D  emcad2
     D  emcad3
     D  emcad4
     D  emcad5
     D cx                                  OVERLAY(cxdt)
     D                                     LIKE(emcad1) DIM(5)
     D                                     inz

     D xxefdt          s                   like(emefd1)
     D xxcxdt          s                   like(emcad1)

     d stsav           s                   like(fmemst)
     d sttmp           s                   like(fmemst)
     d recsav          s                   like(fm2001)
     d rectmp          s                   like(fm2001)

     D qqForm          s             15A   Inz('WC000407  ')

     D addpgm          c                   const('SR00407I  ')

     D SR00407         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR00407         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      * Employer Master
     C     emky1         KList
     C                   KFld                    elco#
     C                   KFld                    elfnd
     C                   KFld                    elemp#
     C                   KFld                    eldiv
      * Endorsement Form Statistical Info
     C     nxkey         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemst
     C                   KFld                    elpfdt
     *
     C     nxkey1        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemst

      /COPY SCOPYEN01C

      *=========================================================================
      *  GetCXDT - Get Cancellation date
      *=========================================================================
     C     GetCXDT       BegSR

     c     emky1         chain     pem01
      *  Get Employer Effective/Cancellation date
     C                   z-add     5             x1                3 0

     C                   dow       ef(x1) = 0
     C                             and (x1 > 1)
     C                   eval      x1 = x1 - 1
     C                   enddo

     C                   eval      xxefdt = ef(x1)
     C                   eval      xxcxdt = cx(x1)

     C                   EndSR
      *=========================================================================
      *  OTHINF - Subroutine to load form specific information
      *=========================================================================
     C     othinf        BegSR
      * for each employer division
     c                   clear                   st
     c                   clear                   rec

     c                   z-add     0             j1                3 0
     c     elky2         chain     pel02                              60

     c                   dow       *in60 = *off
          //***** Beg Add *** 10/26/22 ******************************
             xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
             If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
          //***** End Add *** 10/26/22 ******************************
     c                   exsr      getcxdt
      *   if division not cancelled as of run date
     c                   if        (xxcxdt > xxsydt)
     c                             or (xxcxdt = 0)
      *   if there is a rate change record effective as of policy year
      *   FROM date
     C     rsky          chain     prs01                              60

     C                   if        *in60 = *off
     C                             and rsjrst <> *blanks
     C                             and rsjrst <> 'MO'
      //*** Beg Add ***** 01/27/23 *****************************
       Exsr ChkApprExclSt;
       If isApprExclSt = *On;
      //*** End Add ***** 01/27/23 *****************************
     C                   eval      xxemst = rsjrst
     C     nxkey         SetGT     pnx01
     C     nxkey1        ReadPE    pnx01                                  60

     C                   dow       *in60 = *off
     C                             and nxstat = 'I'
     C     nxkey1        ReadPE    pnx01                                  60
     C                   enddo

     c                   if        (*in60 = *off)
     C                             and (nxrcef = 'Y')
     C                             and (nxrced <> *zero)
     C                             and (nxrced < elptdt)
      *   Write Rating State and Rate Change Date to endorse form
     c                   movel(p)  rsjrst        stsav

     C                   Eval      srdti = nxrced
     C                   Call      'SRCVTDAT'    dtmdyy
     C                   MoveL     srdto         xxdat8            8 0

     c                   eval      recsav = '  ' +
     c                             nxst +
     c                             '             ' +
     c                             %editw(xxdat8:'  /  /    ') +
     c                             '               ' +
     c                             %editc(nxcpct:'K') +
     c                             '        ' +
     c                             %editc(nxlpct:'K')

     c                   exsr      srins

     c                   endif
      //*** Beg Add ***** 01/27/23 *****************************
       Endif;
      //*** End Add ***** 01/27/23 *****************************

     c                   endif

     c                   endif

           //***** Beg Add *** 10/26/22 ******************************
             Endif;
           //***** End Add *** 10/26/22 ******************************
     c     elky2         reade     pel02                                  60
     c                   enddo
      ** Write the output record
     c                   exsr      srout

     C                   EndSR
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     pgmstr        BegSR
     C                   time                    dsdttm
     C                   eval      xx_attach = 'YES'

     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxsydt            7 0

     C                   EndSR
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     pgmend        BegSR
     C                   EndSR
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     valid         BegSR

     c                   if        xx_attach = 'YES'
     C     rsky          chain     prs01                              60

     C                   if        *in60 = *off
     C                             and rsjrst <> *blanks
     c                   exsr      getcxdt

     c                   if        (xxcxdt > xxsydt)
     c                             or (xxcxdt = 0)
     C                   eval      xxemst = rsjrst
     C     nxkey         SetGT     pnx01
     C     nxkey1        ReadPE    pnx01                                  60

     C                   dow       *in60 = *off
     C                             and nxstat = 'I'
     C     nxkey1        ReadPE    pnx01                                  60
     C                   enddo
      ** If a valid record was found, there is a Rate Chg, the Rage Chg date
      ** is not zero, and is less than the current policy TO date.
     C                   If        *in60 = *off
     C                             and nxrcef = 'Y'
     C                             and nxrced <> *zero
     C                             and (nxrced < elptdt)
     C                   Move      *on           crtfrm
     C                   EndIf

     C                   endif

     C                   endif
      *  Do not auto attach if Missouri Coverage on state reporting
      *  divison for policies effective 01/01/08 or later.
     c                   if        rsjrst = 'MO'
     c                             and eldiv = xxdiv
     c                             and elpfdt >= 1080101
     c                   move      *off          crtfrm
     c                   move      'NO '         xx_attach         3
     c                   endif

     c                   endif

     C                   EndSR
      *=========================================================================
      *  srins - Insertion sort
      *=========================================================================
     C     srins         begsr

     c                   z-add     1             x1

     C     x1            Dowle     52

     c                   select

     C     st(x1)        wheneq    *blanks
     c                   movel(p)  stsav         st(x1)
     c                   movel(p)  recsav        rec(x1)
     c                   leave

     C     stsav         whengt    st(x1)
     c                   add       1             x1
     c                   iter

     C     stsav         wheneq    st(x1)
     c                   movel(p)  recsav        rec(x1)
     c                   leave

     C                   other
     c                   movel(p)  st(x1)        sttmp
     c                   movel(p)  rec(x1)       rectmp
     c                   movel(p)  stsav         st(x1)
     c                   movel(p)  recsav        rec(x1)
     c                   movel(p)  sttmp         stsav
     c                   movel(p)  rectmp        recsav
     c                   add       1             x1
     c                   iter

     c                   endsl

     C                   EndDo

     C                   endsr
      *=========================================================================
      * SROUT - Move data from arrays to output fields
      *=========================================================================
     C     srout         begsr

     c                   z-add     1             x1
     c                   z-add     1             j1                3 0
     c                   clear                   output
     C                   move      'N'           xxprint           1

     c                   Dow       st(j1) <> *blanks

     c                   eval      out(x1) = rec(j1)
     c                   add       1             x1
     c                   add       1             j1

     c                   if        x1 = 17

      /Free
          WriteInCopybook = *Off;
          Exsr WriteWorkFile;
      /End-Free

     c                   eval      xxprint = 'Y'
     c                   clear                   output
     c                   eval      x1 = 1
     c                   endif

     c                   enddo

     c                   if        (x1 > 1)
     c                             or ((x1 = 1) and (xxprint = 'N'))

      /Free
          WriteInCopybook = *Off;
          Exsr WriteWorkFile;
      /End-Free

     c                   clear                   output
     c                   endif

     C                   endsr
