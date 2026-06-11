     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR42422 - TX Terrorism Premium Endorsement.                    er Policy Endorsement
      //===============================================================*
      // WC420422  - Texas Terrorism Premium Endorsement.              *
      //                                                               *
      // PROGRAM DESCRIPTION:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //===============================================================*
      // Revisions:                                                    *
      //                                                               *
      // 10/19/22 C:342 Project: WC2834                                *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // 11/08/17 C:33  Project: WC2481   #50264                       *
      //                Apply the LCM against the terrorism rate       *
      //                                                               *
      // 03/07/17 C:180 Project: WC2481                                *
      //                                                               *
      //===============================================================*
      // NOTES:                                                        *
      //                                                               *
      //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
      //                                                               *
      //  qqmode = 1 - Called to determine if form should be attached. *
      //  qqmode = 2 - Called to print endorsement.                    *
      //  qqmode = 3 - Called for additional input.                    *
      //  qqmode = 4 - Called for midterm processing.                  *
      //===============================================================*

      ** Statistical Reporting/Rating File
     Fwdrrl     if   e           k disk
      /COPY SCOPYEN01F

     D SR42422         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR42422         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

     D SRLCM           PR                  EXTPGM('SRLCM')
     D                                3
     D                                3
     D                                3
     D                                5
     D                                7
     D                                5

      /COPY SCOPYEN01D
     d #1co#           s              3    inz(*blanks)
     d #1fnd           s              3    inz(*blanks)
     d ##rtcd          s              3    inz(*blanks)
     d ##clas          s              5    inz(*blanks)
     d ##efdt          s              7    inz(*blanks)
     d ##lcm           s              5    inz(*blanks)
     d xxlcm           s              6    inz

     D qqForm          s             15A   Inz('WC420422  ')
     D addpgm          c                   const('SRXXXXXXXX')

      /COPY SCOPYEN01C

      /Free

       //=========================================================================
       //  OTHINF - Subroutine to load form specific information
       //=========================================================================
       Begsr othinf;

       //Get Client's information

       setll (xxco#:xxfnd:xxfyr:xxemp#) pel02;
       reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;

       Dow not %eof;
          //***** Beg Add *** 10/19/22 ******************************
          xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
          If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
          //***** End Add *** 10/19/22 ******************************
          Chain (Elco#:ELrtcd) prs01;

          If %found(wmrsl) and rsjrst = 'TX';
             setgt (ELco#:ELfnd:RSjrst:ELpfdt) prr01;
             readpe (ELco#:ELfnd:RSjrst) prr01;

             If not %eof(wdrrl) and rrterr <> *zeros;
                #1co#  = %Editc(RRco#:'X');
                #1fnd  = %Editc(RRfnd:'X');
                ##rtcd = ELrtcd;
                ##clas = %Editc(RRterc:'X');
                ##efdt = %Editc(ELpfdt:'X');
                SRLCM(#1co#:#1fnd:##rtcd:##clas:##efdt:##lcm);
                xxlcm = %subst(##lcm:1:1) + '.' + %subst(##lcm:2:4);
                FMcd01 = RSjrst;
              //***  Begin Chg  ***  11/08/17  ****************************
              //FMa541 = %Dec(xxlcm:5:4);
                FMa541 = %Dec(xxlcm:5:4) * RRterr;
              //***  End   Chg  ***  11/08/17  ****************************
                Exsr WriteWorkFile;
             Endif;
          Endif;

          //***** Beg Add *** 10/19/22 ******************************
          Endif;
          //***** End Add *** 10/19/22 ******************************
          reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;
       Enddo;

       Endsr;
       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr Pgmstr;
       WriteInCopybook = *Off;
       Endsr;
       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       Begsr valid;

       Chain (xxco#:elrtcd) prs01;

       If %found(wmrsl) and rsjrst = 'TX' and elpfdt >= 1080101;
          setgt (ELco#:ELfnd:RSjrst:ELpfdt) prr01;
          readpe (ELco#:ELfnd:RSjrst) prr01;

          If not %eof(wdrrl) and rrterc <> *zeros;
             crtfrm = *on;
          Endif;
       Endif;

       Endsr;
       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       Begsr pgmend;
       Endsr;
      /End-Free
