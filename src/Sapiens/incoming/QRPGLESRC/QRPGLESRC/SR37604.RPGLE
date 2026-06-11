     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
    H/TITLE SR37604 - PA - Employer Assessment Endorsement
     *===============================================================*
     * WC370604 - Pennsylvania Employer Assessment Endorsement       *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 02/13/01                                          *
     * Programmer: C:90                                              *
     * Project   : CM027                                             *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
      * 10/19/22 C:342 Project: WC2834                                *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
      * 11/11/19 C:267 Project: WC2695                                *
      *                Write endorsement data record for WC370604     *
      *                in WCPOLS - populate WXPEP record              *
      *               -Adjust factor field to fit a 5 4 field definition
      *                                                               *
     * 09/29/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
     *                                                               *
     * 08/23/01 C:09  PROJECT: CM027    WA107/28711                  *
     *                In "Othinf" subroutine, we need to be sure we  *
     *                are processing the correct div (i.e. where     *
     *                rsjrst = enst).                                *
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
     Fwdrrl     if   e           k disk
     f*** Begin Add ***  11/11/19  *************************************
     FWXPEL     UF a E           K DISK
     FWXPEL1    IF   E           K DISK    RENAME(PPE01:PPE02)
     f*** End Add *****  11/11/19  *************************************
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     f*** Begin Add ***  11/11/19  *************************************
     D/COPY SPRSTRING
     D/Copy wWCPOLDAT4
      //***** Beg Del *** 10/19/22 **********************************
     D*****/Copy wPrEmplr
      //***** End Del *** 10/19/22 **********************************
     D xxunid          s                   like(PEUNID)
     D x1end#          s                   like(PEEND#)
     d xxRateSt        s              2a   inz
     D xxN54           S              5s 4 inz
     f*** End Add *****  11/11/19  *************************************
     ** Copybook D specs
     D/COPY SCOPYEN01D
     D xxwrit          s              1
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     ** Form ID
     D qqForm          s             15a   inz('WC370604  ')
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')

      *entry parameters
     D SR37604         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR37604         PI
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
     f*** Begin Add ***  11/11/19  *************************************
     C     PEKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXDT              7 0

     C     PEKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR

     C     PEKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXUNID
     C                   KFLD                    X1END#
     f*** End Add *****  11/11/19  *************************************
    
     C     rrky          klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    rsjrst
     C                   kfld                    elpfdt
    C*
     C     rrky1         klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    rsjrst
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
     C*** Beg Add *****  11/11/19  *************************************
     C                   CLEAR                   e37604BJ
     C*** Beg Add *****  11/11/19  *************************************

     C                   eval      xxwrit = *off
     C     elky2         chain     pel02                              50
     C                   dow       *in50 = *off and xxwrit = *off
        //***** Beg Add *** 10/19/22 ******************************
        xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
        If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
        //***** End Add *** 10/19/22 ******************************
     *
     C     rsky          chain     prs01                              60
    C*
     C                   if        *in60 = *off
     C                             and rsjrst = enst
     C                             and xxwrit = *off
     *
     C     rrky          setll     prr01
     C     rrky1         readpe    prr01                                  60
    C*
     C                   if        rr1stp <> *zeros
     C                             or rr2ndp <> *zeros
     C                             or rr3rdp <> *zeros
    C*
     C                   if        rr1stp <> *zeros
     C                   eval      fmam01 = el1aam
     C                   movel     rr1stp        xxc101           10
     C*** Beg Add *****  11/11/19  *************************************
     C                   eval      X37604EAA = el1aam
     C                   eval(h)   xxN54     = rr1stp / 100
     C                   movel     xxN54         X37604EAF
     C*** End Add *****  11/11/19  *************************************
     C                   else
     C                   if        rr2ndp <> *zeros
     C                   eval      fmam01 = el2aam
     C                   movel     rr2ndp        xxc101
     C*** Beg Add *****  11/11/19  *************************************
     C                   eval      X37604EAA = el2aam
     C                   eval(h)   xxN54     = rr2ndp / 100
     C                   movel     xxN54         X37604EAF
     C*** End Add *****  11/11/19  *************************************
     C                   else
     C                   eval      fmam01 = el3aam
     C                   movel     rr3rdp        xxc101
     C*** Beg Add *****  11/11/19  *************************************
     C                   eval      X37604EAA = el3aam
     C                   eval(h)   xxN54     = rr3rdp / 100
     C                   movel     xxN54         X37604EAF
     C*** End Add *****  11/11/19  *************************************
     C                   endif
     C*
     C                   endif
     C*
     C                   movel     xxc101        qqnum
     C                   move      '0'           qqthou
     C*
     C                   call      'SA020R'
     C                   parm                    qqnum            80
     C                   parm                    qqmask           10
     C                   parm      6             qqdec             3 0
     C                   parm                    qqthou            1
     C*
     C                   movel     qqnum         fmc101
      /Free
          //* Write the output record
          Exsr WriteWorkFile;
      /End-Free
     C                   eval      xxwrit = *on
     *
     C                   endif
     *
     C                   endif
     *
        //***** Beg Add *** 10/19/22 ******************************
        Endif;
        //***** End Add *** 10/19/22 ******************************
     C     elky2         reade     pel02                                  50
     C                   enddo
     *
     C*** Beg Add *****  11/11/19  *************************************
      /Free

       // Copy data into the WCPOLS record.

       // Get latest UNID
          xxdt = *hival;
          Setgt pekey ppe02;
          Readpe(n) pekey1 ppe02;
          If not %eof;
              xxunid = PEunid;
          Endif;

          x1end# = %trim(qqform);
          Chain pekey2 ppe01;          //Chain for retrieval
          If %found;
             PEData = e37604BJ;
             PEName = %trim(ExtrAlphaNum(PEName));
             Update ppe01;
          Endif;
      /End-Free
     C*** End Add *****  11/11/19  *************************************

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
     C     rrky          setll     prr01
     C     rrky1         readpe    prr01                                  60
     *
     C                   if        rr1stp <> *zeros
     C                             or rr2ndp <> *zeros
     C                             or rr3rdp <> *zeros
     *
     ** Found a division with a matching jurisdiction state
     C                   move      *on           crtfrm
     *
     C                   endif
     *
     C                   endsr
