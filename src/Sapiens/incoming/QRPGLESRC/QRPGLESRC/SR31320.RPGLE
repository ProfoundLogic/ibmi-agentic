     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
    H/TITLE SR31320 - NY - Optional Labor Contractor Endorsement
     *===============================================================*
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 12/08/05  C:61  Project: 139WBU                   *
     *===============================================================*
     * REVISIONS:
     *
     * 10/19/22 C:342 Project: WC2834                                *
     *                Endorsement Recognition of Cancellation        *
     *                                                               *
     * 10/06/17 C:136 Project: 150WBL
     *                PEO attachment criteria moved to copy code
     *
      * 07/25/17 C:180 Project: 150WAP
      *                Handle groups with combined PEOs.
      *
     * 03/02/17 C:136 Project: WC2481 (Rewrite)
     *                Print all divisions regardless of PEO output
     *                control flag. Only attach if NY client exists.
     *
     * 04/28/06 C:61  support cm027
     *                Attach to policies active in the future.
     *
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
     * Copybook F specs
     F/COPY SCOPYEN01F
    
      ** Statistical Reporting/Rating File
     Fwdrrl     if   e           k disk
     *=========================================================================
     *  Definition Specs
     *=========================================================================
     d intCxDate7      s              7  0

     * Copybook D specs
     D/COPY SCOPYEN01D
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     * Form ID
     D qqForm          s             15a   inz('WC310320  ')

     * Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
     *=========================================================================
     *  Prototypes
     *=========================================================================
      *Entry parameters
     D SR31320         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR31320         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      //***** Beg Del *** 10/19/22 **********************************
     D******/Copy WprEmplr
      //***** End Del *** 10/19/22 **********************************
     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
     C     *like         define    rpemp#        x1emp#
     C     *like         define    rpdiv         x1div
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     C     rrky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    svjrst
     C                   kfld                    elpfdt
      *
     C     rrky1         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    svjrst

     C     rpky1         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    x1emp#
     C                   KFld                    x1div
     C                   KFld                    xxfyr
     *=========================================================================
     *  Main
     *=========================================================================
     * Copybook C specs
     C/COPY SCOPYEN01C
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
    
     ** Get PEO Policy Number
    
     c                   z-add     1             x1emp#
     c                   z-add     0             x1div
     c                   open      wmrpl
     c     rpky1         chain     prp01                              60
    
     c                   if        (*in60 = *off) and (rpcap# <> *blanks)
     c                   movel(p)  rpcap#        fmpol#
    
     c                   else
    
     c                   movel(p)  qqfnd         fmpol#
     c                   movel(p)  x1emp#        xxtmp             9
     c                   cat       xxtmp:0       fmpol#
     c                   movel(p)  xxfyr         xxtmp             9
     c                   cat       xxtmp:0       fmpol#
     c                   endif
    
     c                   close     wmrpl
    
     * write endorsements
    
     c                   exsr      peogrp
    
     c                   endsr
     *=========================================================================
     *  PEOGRP - Run through WDELP for NY divisions. Print those not flat cx'd
     *=========================================================================
      /Free
       Begsr PeoGrp;
       Setll (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
       Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
       Dow Not(%Eof);
          Chain (Elco#:ELrtcd) prs01;
          Select;
          When FNpeof = 'M' and ELemp# = 1;
          //*** Begin Add *** 07/25/17 **************************************
          When FNpeof = 'C' and Elemp# = 1;
          //*** End   Add *** 07/25/17 **************************************

          Other;
             If RSjrst = ENst;
                Chain (ELco#:ELfnd:ELemp#:ELdiv) pem01;
                If ELpfdt <> GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
                   Exsr PeoWrt;
                Endif;
             Endif;
          Endsl;
       Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
       Enddo;
       Endsr;
      /End-Free
     *=========================================================================
     *  PEOWRT - PEO GROUP - Write Endorsement
     *=========================================================================
     c     peowrt        begsr

     c                   clear                   fmnm01
     c                   clear                   fmnm02
     c                   clear                   fmead1
     c                   clear                   fmead2
     c                   clear                   fmead3

     c                   eval      fmnm01 = emname
     c                   eval      fmnm02 = 'FEIN: ' +  %editc(emfein:'X')

     c                   If        empad1 <> *blanks
     c                   eval      fmead1 = empad1
     c                   eval      fmead2 = empad2
     c                   move      empzip        xxzip
     c     empcty        cat       ',':0         fmead3
     c                   cat       empst:1       fmead3
     c                   else
     c                   eval      fmead1 = emmad1
     c                   eval      fmead2 = emmad2
     c                   move      emmzip        xxzip
     c     emmcty        cat       ',':0         fmead3
     c                   cat       emmst:1       fmead3
     c                   endif
    
     c                   cat       xxzip1:1      fmead3
     c                   if        xxzip2 <> *blank
     c                             and xxzip2 <> *zero
     c                   cat       '-':0         fmead3
     c                   cat       xxzip2:0      fmead3
     c                   endif
    
      /Free
          //* Write the output record
          Exsr WriteWorkFile;
      /End-Free

     c                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
      /Free
       Begsr pgmstr;
          WriteInCopybook = *Off;
       Endsr;
      /End-Free
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
     *=========================================================================
     *  Valid - Attach Criteria
     *=========================================================================
      /Free
       Begsr Valid;
       crtFrm = *On;
       Setgt  rrKy prr01;
       Readpe rrKy1 prr01;
       intCxDate7 = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
       If RRpgfl = *blank or ELpfdt = intCxDate7;
          crtFrm = *Off;
       Endif;
       Endsr;
      /End-Free
