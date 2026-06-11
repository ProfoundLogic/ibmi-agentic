     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
    H/TITLE SR04422 - CA - Retrospective Premium Endorsement
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 02/16/12                                          *
     * Programmer: C:282                                             *
      * Project   : WC2120
     *===============================================================*
     * REVISIONS:                                                    *
      *                                                               *
     * 10/31/22 C:333 Project: WC2834                                *
      *                Endorsement Recognition of Cancellation
     *                                                               *
     * 09/15/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
     *                                                               *
     * 02/19/14 C:190 Project WC2246 Priss#10                        *
     *                Corrected out of index error.                  *
     *                                                               *
     * 01/27/14 Project : WC2246                                     *
     *          Handling days with common short rate percentage      *
     *          values as a range of days.                           *
     *                                                               *
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
      ** Short Rate Detail File (CO#/FND/ST/EFDT/DAYS)
    FWDSTL     if   e           k disk
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     ** Copybook D specs
     D/COPY SCOPYEN01D
    

      * employer effective dates
     d edds            ds
     d  emefd1
     d  emefd2
     d  emefd3
     d  emefd4
     d  emefd5
    d ed                                  OVERLAY(edds)
     d                                     LIKE(emefd1) DIM(5)

      * employer cancel dates
     d cdds            ds
     d  emcad1
     d  emcad2
     d  emcad3
     d  emcad4
     d  emcad5
    d cd                                  OVERLAY(cdds)
     d                                     LIKE(emcad1) DIM(5)
     d
     dcounter          s              2  0
     dkkco#            s              3  0
     dkkfnd            s              3  0
     dkkst             s              2
     dkkefdt           s              7  0
     dtmpefdt          s              7  0
     D print           s               n   inz(*off)
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqForm          s             15a   inz('WC040422  ')
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
     **
     D  FM1ds          ds
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
     D  fm2017
     D  fm2018
     D  fm2019
     D  fm2020
     D  fm2021
     D  fm2022
     D  fm2023
     D  fm2024
     D  fm2025
     D  fm2026
     D  fm2027
     D  fm2028
     D  fm2029
     D  fm2030
     D  fm2031
     D  fm2032
    D fm1                                 OVERLAY(fm1ds)
     D                                     LIKE(fm2001) DIM(32)
      * counters
     d x1              s              3  0
     d x2              s              3  0
      * temp
     d xxdays          s              7
     d xxdaysFrm       s              3  0
     d svdays          s              3  0
     d svperc          s              5  2
     d intPos          s              1  0
     d intLen          s              1  0
     d charMonth       s             10
     d intMon          s              2  0
     d intMonR         s              2  0
     d xxperc          s              5  2

     *entry parameters
     D SR04422         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR04422         PI
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
    
     * Employer Master
    
     C     emky1         klist
     C                   KFld                    elco#
     C                   KFld                    elfnd
     C                   KFld                    elemp#
     C                   KFld                    eldiv
     *
     * Short rate deatil table file
     *
     C     stky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkst
     C                   kfld                    kkefdt
     *
     C     stky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkst
    
     c     elky1         klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELFYR
     c                   kfld                    ELEMP#
     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs
     C/COPY SCOPYEN01C
    
     *=========================================================================
     *  CK_EMP_CX - Check if employer division is cancelled
     *=========================================================================
     c     ck_emp_cx     begsr

     c                   move      'Y'           emp_cx            1

     c     emky1         chain     pem01

     c                   if        %found

      *  Get Employer Effective/Cancellation date

     C                   z-add     5             x                 3 0

     c                   dow       (ed(x) = 0) and (x > 1)
     c                   sub       1             x
     c                   enddo

     c                   if        (cd(x) = 0) or (cd(x) > sysdat)
     c                   eval      emp_cx = 'N'
     c                   endif

     c                   endif

     c                   endsr
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
     *
     c                   eval      print = *Off
     c     elky1         setll     pel02
     c     elky1         reade     pel02                                  60
     c                   Dow       *in60 = *off
       //***** Beg Add *** 10/31/22 ******************************
          xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
          If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
       //***** End Add *** 10/31/22 ******************************
     c     rsky          chain     prs01                              60
     c                   If        (*in60 = *off and
     c                              rsjrst = 'CA')
     c                   eval      Print = *On
     c                   Leave
     c                   Endif
          //***** Beg Add *** 10/31/22 ******************************
             Endif;
          //***** End Add *** 10/31/22 ******************************
     c     elky1         reade     pel02                                  60
     c                   Enddo

     c                   If        Print = *On

      *
     * Short rate information records exists
     c                   move      qqco#         kkco#
     c                   move      qqfnd         kkfnd
     c                   move      'CA'          kkst
     c                   move      elpfdt        kkefdt
     *
     c     stky          setgt     pst01
     c     stky1         readpe    pst01                                  60
    c                   if        (*in60 = *off and
    c                              stperc <> *zero)
     c                   move      stefdt        tmpefdt
     C                   endif                                                  (*in60 = *off and
     *
      * Counter to increment the record upto 96 record.
     c                   eval      x1 = 1
     *
      * Counter to increment the record upto 32 record.
     c                   eval      x2 = 1
     *
     c                   move      tmpefdt       kkefdt
     c     stky          setll     pst01
     c     stky          reade     pst01                                  60
     *
     c                   dow       (*in60 = *off
    c                              and stperc <> *zero)
     *
      *
     c                   if        stperc <> svperc and svperc <> 0

     c                   if        x1 <=32
     c                   eval      fm1(x2) = xxdays  +
     c                                       charMonth    +
     c                                       %editc(xxperc:'4')
     c                   endif
      *
     c                   if        x1 >32 and x1<=64
     c                   eval      fm1(x2) = %Subst(fm1(x2):1:23) +
     c                                       '     ' +
     c                                       xxdays   +
     c                                       charMonth    +
     c                                       %editc(xxperc:'4')
     c                   endif
      *
     c                   if        x1 >64 and x1<=96
     c                   eval      fm1(x2) = %Subst(fm1(x2):1:51) +
     c                                       '     ' +
     c                                       xxdays   +
     c                                       charMonth    +
     c                                       %editc(xxperc:'4')
     c                   endif
      *
      * If record is read upto 32 record than reset the x2 counter to 1
     c                   if        x2 = 32
     c                   eval      x2 = 1
     c                   else
     c                   eval      x2 = x2 + 1
     c                   endif
      *
      * If record is read upto 96 record than write the record in file wxfmp.
     c                   if        x1 = 96
      /Free
       //* Write the output record
       WriteInCopybook = *Off;
       Exsr WriteWorkFile;
      /End-Free
     c                   clear                   fm1
     c                   eval      x1 = 1
     c                   else
     c                   eval      x1 = x1 + 1
     c                   endif
      * Clear the fields.
     c                   clear                   xxdaysFrm
     c                   clear                   xxdays
     c                   clear                   charMonth
     c                   endif
      *
      * Cumulate the Days in Range if they have the same %.
     c                   if        stperc = svperc
      *
     c                   if        xxdaysFrm = 0
     c                   move      svdays        xxdaysFrm
     c                   endif
      *
     c                   if        xxdaysFrm <> stdays
     c                   eval      xxdays = %trim(%char(xxdaysFrm)) + '-' +
     c                                      %trim(%char(stdays))
     c                   endif
     c                   if        charMonth = *Blank
     c     svdays        div       30            intMon
     c                   Mvr                     intMonR
     c                   if        intMonR = 0 and svdays >= 30
     c                   eval      charMonth = ' (' + %Char(intMon) + 'Mos.)'
     c                   endif
     c                   if        intMonR = 0 and intMon = 1
     c                   eval      charMonth = ' (' + %Char(intMon) + 'Mo.)'
     c                   endif
     c                   endif
     c                   else
     c                   eval      xxdays   = %trim(%char(stdays))
     c                   move      stperc        xxperc
     c                   if        charMonth = *Blank
     c     svdays        div       30            intMon
     c                   Mvr                     intMonR
     c                   if        intMonR = 0 and svdays >= 30
     c                   eval      charMonth = ' (' + %Char(intMon) + 'Mos.)'
     c                   endif
     c                   if        intMonR = 0 and intMon = 1
     c                   eval      charMonth = ' (' + %Char(intMon) + 'Mo.)'
     c                   endif
     c                   endif
     c                   endif
      *
      * Populate the % and Days in a temp variable.
     c                   move      stperc        svperc
     c                   move      stdays        svdays
      /Free
       intLen = %Len(xxdays);
       intPos = %Scan(' ':xxdays:1);
       If intPos <> 0;
          xxdays = %SubSt(xxdays:intpos:intLen - (intPos-1)) +
                   %SubSt(xxdays:1:intPos-1);
       EndIf;
      /End-Free
      *
     c     stky          reade     pst01                                  60
     c                   enddo
     *
      ** Write the output record.
     c                   if        x1 <=32
     c                   eval      fm1(x2) = xxdays  +
     c                                       charMonth    +
     c                                       %editc(xxperc:'4')
     c                   endif
      *
     c                   if        x1 >32 and x1<=64
     c                   eval      fm1(x2) = %Subst(fm1(x2):1:23) +
     c                                       '     ' +
     c                                       xxdays   +
     c                                       charMonth    +
     c                                       %editc(xxperc:'4')
     c                   endif
      *
     c                   if        x1 >64 and x1<=96
     c                   eval      fm1(x2) = %Subst(fm1(x2):1:51) +
     c                                       '     ' +
     c                                       xxdays   +
     c                                       charMonth    +
     c                                       %editc(xxperc:'4')
     c                   endif
      *
     c                   if        fm1(x2) <> *Blanks
      /Free
       //* Write the output record
       WriteInCopybook = *Off;
       Exsr WriteWorkFile;
      /End-Free
     c                   endif
     c                   Endif                                                  Print = *On
     *
     c                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
      *
     c                   time                    dsdttm
     c                   Eval      srdti = dsdate
     c                   Call      'SRCVTDAT'    dtcymd
     c                   MoveL     srdto         sysdat            7 0
      *
     c                   endsr
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
     c     valid         begsr
    
     c     rsky          chain     prs01                              60
    
     c                   if        (*in60 = *off and
     c                              rsjrst = 'CA')

     * Check that employer division is not cancelled
     c                   exsr      ck_emp_cx
     *
     c                   if        emp_cx = 'N'
     *
     * Short rate information records exists
     c                   move      qqco#         kkco#
     c                   move      qqfnd         kkfnd
     c                   move      'CA'          kkst
     c                   move      elpfdt        kkefdt
     *
     c     stky          setgt     pst01
     c     stky1         readpe    pst01                                  60
    c                   if        (*in60 = *off and
    c                              stperc <> *zero)
     c                   move      stefdt        tmpefdt
     C                   move      *on           crtfrm
     C                   endif                                                  (*in60 = *off and
     *                                                                          stperc <> *zero)
     c                   endif                                                  emp_cx = 'N'
     *
     c                   endif                                                  (*in60 = *off  and
     *                                                                          rsjrst = 'CA')
     *
     c                   endsr
    
